use warnings;
use strict;

package XML::Compile::WSDL11;
use base 'XML::Compile::Cache';

use Log::Report 'xml-compile-soap', syntax => 'SHORT';

use XML::Compile             ();      
use XML::Compile::Util       qw/pack_type unpack_type/;
use XML::Compile::SOAP       ();
use XML::Compile::SOAP::Util qw/:wsdl11/;
use XML::Compile::SOAP::Extension;

use XML::Compile::SOAP::Operation  ();
use XML::Compile::Transport  ();

use File::Spec     ();
use List::Util     qw/first/;
use Scalar::Util   qw/blessed/;
use File::Basename qw/dirname/;

=chapter NAME

XML::Compile::WSDL11 - create SOAP messages defined by WSDL 1.1

=chapter SYNOPSIS

 # preparation
 use XML::Compile::WSDL11;      # use WSDL version 1.1
 use XML::Compile::SOAP11;      # use SOAP version 1.1
 use XML::Compile::Transport::SOAPHTTP;

 # you want some trace?
 use Log::Report mode => 'DEBUG';   # or 'VERBOSE'

 # collect one or more wsdls and xsds in one object
 my $wsdl = XML::Compile::WSDL11->new($wsdlfile
   , server_type => 'BEA'
   );
 $wsdl->addWSDL(...more WSDL files...);
 $wsdl->importDefinitions(...more schemas...);

 # during initiation, for each used call
 my $call = $wsdl->compileClient('GetStockPrice', ...);

 # at "run-time", call as often as you want (fast)
 my $answer = $call->(%request);

 # capture useful trace information
 my ($answer, $trace) = $call->(%request);
 if($trace->errors)
 {   $trace->printError;
 }

 # no need to administer the operations by hand: alternative
 $wsdl->compileCalls;  # at initiation
 my $answer = $wsdl->call(GetStockPrice => %request);

 # investigate the %request structure (server input)
 print $wsdl->explain('GetStockPrice', PERL => 'INPUT', recurse => 1);

 # investigate the $answer structure (server output)
 print $wsdl->explain('GetStockPrice', PERL => 'OUTPUT');

 # when you like, get all operation definitions
 my @all_ops = $wsdl->operations;

 # Install XML::Compile::SOAP::Daemon
 my $server  = XML::Compile::SOAP::HTTPDaemon->new;
 $server->operationsFromWSDL($wsdl);
 undef $wsdl;    # not needed any further
 
 # For debug info, start your script with:
 use Log::Report mode => 'DEBUG';

=chapter DESCRIPTION

This module understands WSDL version 1.1.  An WSDL file defines a set of
messages to be send and received over (SOAP) connections. This involves
encoding of the message to be send into XML, sending the message to the
server, collect the answer, and finally decoding the XML to Perl.

As end-user, you do not have to worry about the complex details of the
messages and the way to exchange them: it's all simple Perl for you.
Also, faults are handled automatically.  The only complication you have
to worry about is to shape a nested HASH structure to the sending
message structure.  M<XML::Compile::Schema::template()> may help you.

When the definitions are spread over multiple files you will need to
use M<addWSDL()> (wsdl) or M<importDefinitions()> (additional schema's)
explicitly. Usually, interreferences between those files are broken.
Often they reference over networks (you should never trust). So, on
purpose you B<must explicitly load> the files you need from local disk!
(of course, it is simple to find one-liners as work-arounds, but I will
to tell you how!)

=chapter METHODS

=section Constructors

=c_method new $xml, %options
The $xml is the WSDL file, which is anything accepted by
M<XML::Compile::dataToXML()>.

=option  server_type NAME
=default server_type C<undef>
[3.01] When you specify the implementation of the server, we can work
around some of the known bugs and limitation.
Read M<XML::Compile::SOAP> section "Supported servers" for supported
servers.
=cut

sub init($)
{   my ($self, $args) = @_;
    $args->{schemas} and panic "new(schemas) option removed in 0.78";
    my $wsdl = delete $args->{top};

    local $args->{any_element}      = 'ATTEMPT';
    local $args->{any_attribute}    = 'ATTEMPT'; # not implemented
    local $args->{allow_undeclared} = 1;

    $self->SUPER::init($args);

    $self->{index}   = {};

    $self->addPrefixes(wsdl => WSDL11, soap => WSDL11SOAP, http => WSDL11HTTP);

    # next modules should change into an extension as well...
    $_->can('_initWSDL11') && $_->_initWSDL11($self)
        for XML::Compile::SOAP->registered;

    XML::Compile::SOAP::Extension->wsdl11Init($self, $args);

    $self->declare
      ( READER      => 'wsdl:definitions'
      , key_rewrite => 'PREFIXED(wsdl,soap,http)'
      , hook        => {type => 'wsdl:tOperation', after => 'ELEMENT_ORDER'}
      );

    $self->{XCW_dcopts} = {};
    $self->{XCW_server} = $args->{server_type};

    my @xsds = map File::Spec->catdir(dirname(__FILE__), 'WSDL11', 'xsd', $_)
      , qw(wsdl.xsd wsdl-mime.xsd wsdl-http.xsd);

    $self->importDefinitions(\@xsds, element_form_default => 'qualified');

    $self->addWSDL($_) for ref $wsdl eq 'ARRAY' ? @$wsdl : $wsdl;
    $self;
}

sub schemas(@) { panic "schemas() removed in v2.00, not needed anymore" }

#--------------------------

=section Accessors

=section Compilers

=method compileAll [ <'READERS'|'WRITERS'|'RW'|'CALLS'>, [$ns] ]
[2.20] With explicit C<CALLS> or without any parameter, it will call
M<compileCalls()>. Otherwise, see M<XML::Compile::Cache::compileAll()>.
=cut

sub compileAll(;$$)
{   my ($self, $need, $usens) = @_;
    $self->SUPER::compileAll($need, $usens)
        if !$need || $need ne 'CALLS';

    $self->compileCalls
        if !$need || $need eq 'CALLS';
    $self;
} 

=method compileCalls %options
[2.20] Compile a handler for each of the available operations. The %options are
passed to each call of M<compileClient()>, but will be overruled by more
specific declared options.

Additionally, %options can contain C<service>, C<port>, and C<binding>
to limit the set of involved calls. See M<operations()> for details on
these options.

You may declare additional specific compilation options with the
M<declare()> method.

=option  long_names BOOLEAN
=default long_names C<false>
[3.03] Prepend the service name to the operation name to produce an alias
(see M<compileCall(alias)>) for the compiled call.  If the service name is
'X' and the operation name is 'Y', then the alias becomes 'X#Y'.

You will need this if you have multiple operations with the same name
in your WSDL (-collection).

=example
   my $trans = XML::Compile::Transport::SOAPHTTP
     ->new(timeout => 500, address => $wsdl->endPoint);
   $wsdl->compileCalls(transport => $trans);

   # alternatives for simple cases
   $wsdl->compileAll('CALLS');
   $wsdl->compileAll;
   
   my $answer = $wsdl->call($myop, $request);
=cut

sub compileCalls(@)
{   my ($self, %args) = @_;
    my $long = $args{long_names};

    my @ops = $self->operations
      ( service => delete $args{service}
      , port    => delete $args{port}
      , binding => delete $args{binding}
      );

    foreach my $op (@ops)
    {   my $alias = $long ? $op->longName : undef;
        $self->compileCall($op, alias => $alias, %args);
    }

    $self;
}

=method compileCall $operation|$opname, %options
[2.37] The call to the $operation object (which extends
M<XML::Compile::SOAP::Operation>) gets compiled and cached so it can
be used with M<call()>.

[2.38] Alteratively to an $operation object, you may also specify an
operation by name.

=option  alias NAME
=default alias C<undef>
[3.03] When defined, the compiled operation will be stored with the
alias name in stead of the operation name.  This may make your code
more readible or solve naming conflicts.  See M<compileCall(prefixed)>

=example
  my $op = $wsdl->operation(name => 'getInfo');
  $wsdl->compileCall($op);

  # as often as you need it
  my ($answer, $trace) = $wsdl->call('getInfo')->(%request);

=cut

sub compileCall($@)
{   my ($self, $oper, %opts) = @_;
    my $alias = delete $opts{alias};
    my $op    = blessed $oper ? $oper : $self->operation($oper, %opts);

    my $name  = $alias || $op->name;
    error __x"a compiled call for {name} already exists", name => $name
        if $self->{XCW_ccode}{$name};

    my $dopts = $self->{XCW_dcopts} || {};
    my @opts  = %opts;
    push @opts, ref $dopts eq 'ARRAY' ? @$dopts : %$dopts;
    trace "compiling call `$name'";
    $self->{XCW_ccode}{$name} = $op->compileClient(@opts);
}

=method call $opname, $data
[2.20] Call the $opname (operation name) with $data (HASH or LIST of parameters).
This only works when you have called M<compileCalls()> beforehand,
always during the initiation phase of the program.

=example
   # at initiation time (compile once)
   $wsdl->compileCalls;

   # at runtime (run often)
   my $answer = $wsdl->call($operation, $request);
=cut

sub call($@)
{   my ($self, $name) = (shift, shift);

    my $codes = $self->{XCW_ccode}
        or error __x"you can only use call() after compileCalls()";

    my $call  = $codes->{$name}
        or error __x"operation `{name}' is not known", name => $name;
    
    $call->(@_);
}

#--------------------------

=section Extension

=method addWSDL $xmldata, %options
The $xmldata must be acceptable to M<XML::Compile::dataToXML()> and 
should represent the top-level of a (partial) WSDL document.
The specification can be spread over multiple files, each of
which must have a C<definition> root element.

=cut

sub addWSDL($%)
{   my ($self, $data, %args) = @_;
    defined $data or return ();

    my ($node, %details) = $self->dataToXML($data);
    defined $node or return $self;

    $node->localName eq 'definitions' && $node->namespaceURI eq WSDL11
        or error __x"root element for WSDL is not 'wsdl:definitions'";

    $self->importDefinitions($node, details => \%details);
    $self->learnPrefixes($node);

    my $spec = $self->reader('wsdl:definitions')->($node);
    my $tns  = $spec->{targetNamespace}
        or error __x"WSDL sets no targetNamespace";

    # WSDL 1.1 par 2.1.1 says: WSDL def types each in own name-space
    my $index     = $self->{index};

    # silly WSDL structure
    my $toplevels = $spec->{gr_wsdl_anyTopLevelOptionalElement} || [];

    foreach my $toplevel (@$toplevels)
    {   my ($which, $def) = %$toplevel;        # always only one
        $which =~ s/^wsdl_(service|message|binding|portType)$/$1/
            or next;

        $index->{$which}{pack_type $tns, $def->{name}} = $def;

        if($which eq 'service')
        {   foreach my $port ( @{$def->{wsdl_port} || []} )
            {   my $addr_label = first { /_address$/ } keys %$port
                    or error __x"no address in port {port}"
                        , port => $port->{name};
                my $first_addr = $port->{$addr_label};
                $first_addr    = $first_addr->[0] if ref $first_addr eq 'ARRAY';

                # Is XML::Compile::SOAP<version> loaded?
                ref $first_addr eq 'HASH'
                    or error __x"soap namespace {ns} not loaded"
                       , ns => $first_addr->namespaceURI;

                $index->{port}{pack_type $tns, $port->{name}} = $port;
            }
        }
    }

    # no service block when only one port
    unless($index->{service})
    {   # only from this WSDL, cannot use collective $index
        my @portTypes = map $_->{wsdl_portType}||(), @$toplevels;
        @portTypes==1
            or error __x"no service definition so needs 1 portType, found {nr}"
                 , nr => scalar @portTypes;

        my @bindings = map $_->{wsdl_binding}||(), @$toplevels;
        @bindings==1
            or error __x"no service definition so needs 1 binding, found {nr}"
                 , nr => scalar @bindings;

        my $binding  = pack_type $tns, $bindings[0]->{name};
        my $portname = $portTypes[0]->{name};
        my $servname = $portname;
        $servname =~ s/Service$|(?:Service)?Port(?:Type)?$/Service/i
             or $servname .= 'Service';

        my %port = (name => $portname, binding => $binding
           , soap_address => {location => 'http://localhost'} );

        $index->{service}{pack_type $tns, $servname}
            = { name => $servname, wsdl_port => [ \%port ] };
        $index->{port}{pack_type $tns, $portname} = \%port;
    }
#warn "INDEX: ",Dumper $index;
    $self;
}

=method namesFor $class
Returns the list of names available for a certain definition $class in
the WSDL. See M<index()> for a way to determine the available $class
information.

=cut

sub namesFor($)
{   my ($self, $class) = @_;
    keys %{shift->index($class) || {}};
}

=method operation [$name], %options
Collect all information for a certain operation.  Returned is an
M<XML::Compile::SOAP::Operation> object.

An operation is defined by a service name, a port, some bindings,
and an operation name, which can be specified explicitly and is often
left-out: in the many configurations where there are no alternative
choices. In case there are alternatives, you will be requested to
pick an option.

=option  service QNAME|PREFIXED
=default service <only when just one service in WSDL>
Required when more than one service is defined.

=option  port NAME
=default port <only when just one port in WSDL>
Required when more than one port is defined.

=option  action STRING
=default action <undef>
Overrule the soapAction from the WSDL.

=requires operation NAME
Ignored when the parameter list starts with a $name (which is an
alternative for this option).  Optional when there is only
one operation defined within the portType.

=option  server_type NAME
=default server_type C<undef>
Overrules M<new(server_type)>.
=cut

# new options, then also add them to the list in compileClient()

sub operation(@)
{   my $self = shift;
    my $name = @_ % 2 ? shift : undef;
    my %args = (name => $name, @_);

    #
    ## Service structure
    #

    my $service   = $self->findDef(service => delete $args{service});

    my $port;
    my @ports     = @{$service->{wsdl_port} || []};
    if(my $not = first {blessed $_} @ports)
    {   error __x"not all name-spaces loaded, {ns} not parsed in port"
          , ns => $not->namespaceURI;
    }

    my @portnames = map $_->{name}, @ports;
    if(my $portname = delete $args{port})
    {   $port = first {$_->{name} eq $portname} @ports;
        error __x"cannot find port `{portname}', pick from {ports}"
            , portname => $portname, ports => join("\n    ", '', @portnames)
           unless $port;
    }
    elsif(@ports==1)
    {   $port = shift @ports;
    }
    else
    {   error __x"specify port explicitly, pick from {portnames}"
            , portnames => join("\n    ", '', @portnames);
    }

    # get plugin for operation
    my $address   = first { /address$/ && $port->{$_}{location}} keys %$port
        or error __x"no address provided in service {service} port {port}"
             , service => $service->{name}, port => $port->{name};

    if($address =~ m/^{/)      # }
    {   my ($ns)  = unpack_type $address;

        warning __"Since v2.00 you have to require XML::Compile::SOAP11 explicitly"
            if $ns eq WSDL11SOAP;

        error __x"ports of type {ns} not supported (not loaded?)", ns => $ns;
    }

#use Data::Dumper;
#warn Dumper $port, $self->prefixes;
    my ($prefix)  = $address =~ m/(\w+)_address$/;
    $prefix
        or error __x"port address not prefixed; probably need to add a plugin XML::Compile::SOAP12";

    my $opns      = $self->findName("$prefix:");
    my $protocol  = XML::Compile::SOAP->plugin($opns);
    unless($protocol)
    {   my $pkg = $opns eq WSDL11SOAP   ? 'SOAP11'
                : $opns eq WSDL11SOAP12 ? 'SOAP12'
                :                         undef;

        if($pkg)
        {   error __x"add 'use XML::Compile::{pkg}' to your script", pkg=>$pkg;
        }
        else
        {   notice __x"ignoring unsupported namespace {ns}", ns => $opns;
            return;
        }
    }

    my $opclass = $protocol.'::Operation';
    $opclass->can('_fromWSDL11')
        or error __x"WSDL11 not supported by {class}", class => $opclass;

    #
    ## Binding
    #

    my $bindtype  = $port->{binding}
        or error __x"no binding defined in port '{name}'"
               , name => $port->{name};

    my $binding   = $self->findDef(binding => $bindtype);

    my $type      = $binding->{type}  # get portTypeType
        or error __x"no type defined with binding `{name}'"
               , name => $bindtype;

    my $portType  = $self->findDef(portType => $type);
    my $types     = $portType->{wsdl_operation}
        or error __x"no operations defined for portType `{name}'"
               , name => $type;

    my @port_ops  = map $_->{name}, @$types;

    $name       ||= delete $args{operation};
    my $port_op;
    if(defined $name)
    {   $port_op = first {$_->{name} eq $name} @$types;
        error __x"no operation `{op}' for portType {pt}, pick from{ops}"
          , op => $name, pt => $type, ops => join("\n    ", '', @port_ops)
            unless $port_op;
    }
    elsif(@port_ops==1)
    {   $port_op = shift @$types;
        $name    = $port_op->{name};
    }
    else
    {   error __x"multiple operations in portType `{pt}', pick from {ops}"
            , pt => $type, ops => join("\n    ", '', @port_ops)
    }

    my @bindops   = @{$binding->{wsdl_operation} || []};
    my $bind_op   = first {$_->{name} eq $name} @bindops;
    $bind_op
        or error __x"cannot find bind operation for {name}", name => $name;

    # This should be detected while parsing the WSDL because the order of
    # input and output is significant (and lost), but WSDL 1.1 simplifies
    # our life by saying that only 2 out-of 4 predefined types can actually
    # be used at present.

    my @order = map +(unpack_type $_)[1], @{$port_op->{_ELEMENT_ORDER}};

    my ($first_in, $first_out);
    for(my $i = 0; $i<@order; $i++)
    {   $first_in  = $i if !defined $first_in  && $order[$i] eq 'input';
        $first_out = $i if !defined $first_out && $order[$i] eq 'output';
    }

    my $kind
      = !defined $first_in     ? 'notification-operation'
      : !defined $first_out    ? 'one-way'
      : $first_in < $first_out ? 'request-response'
      :                          'solicit-response';

    #
    ### message components
    #

    my $operation = $opclass->_fromWSDL11
     ( name      => $name,
     , kind      => $kind

     , service   => $service
     , serv_port => $port
     , binding   => $binding
     , bind_op   => $bind_op
     , portType  => $portType
     , port_op   => $port_op

     , wsdl      => $self
     , action    => $args{action}

     , server_type => $args{server_type} || $self->{XCW_server}
     );
 
    $operation;
}

=method compileClient [$name], %options
Creates an M<XML::Compile::SOAP::Operation> temporary object using
M<operation()>, and then calls C<compileClient()> on that.  This
results in a code reference which will handle all client-server
SOAP exchange.

The %options available include all of the options for:
=over 4
=item *
M<operation()> (i.e. C<service> and C<port>), and all of
=item *
M<XML::Compile::SOAP::Operation::compileClient()> (there are many of
these, for instance C<transport_hook> and C<server>)
=back

You B<cannot> pass options for M<XML::Compile::Schema::compile()>, like
C<<sloppy_integers => 0>>, hooks or typemaps this way. Use M<new(opts_rw)>
and friends to declare those.

When you use M<compileCall()>, the compiled code references get cached
for you.  In that case, you can use M<call()> to use them.

=example
  my $call = $wsdl->compileClient
    ( operation => 'HelloWorld'
    , port      => 'PrefillSoap' # only required when multiple ports
    );
  my ($answer, $trace) = $call->($request);

  # 'operation' keyword optional
  my $call = $wsdl->compileClient('HelloWorld');
=cut

sub compileClient(@)
{   my $self = shift;
    unshift @_, 'operation' if @_ % 2;
    my $op   = $self->operation(@_) or return ();

    my $dopts = $self->{XCW_dcopts} || {};
    $op->compileClient(@_, (ref $dopts eq 'ARRAY' ? @$dopts : %$dopts));
}

#---------------------

=section Administration

=method declare $group, $component|ARRAY, %options
Register specific compile %options for the specific $component. See also
M<XML::Compile::Cache::declare()>. The $group is either C<READER>,
C<WRITER>, C<RW> (both reader and writer), or C<OPERATION>.  As $component,
you specify the element name (for readers and writers) or operation name
(for operations). %options are specified as LIST, ARRAY or HASH.

=example
   $wsdl->declare(OPERATION => 'GetStockPrice', @extra_opts);
   $wsdl->compileCalls;
   my $answer = $wsdl->call(GetStockPrice => %request);
=cut

sub declare($$@)
{   my ($self, $need, $names, @opts) = @_;
    my $opts = @opts==1 ? shift @opts : \@opts;
    $opts = [ %$opts ] if ref $opts eq 'HASH';

    $need eq 'OPERATION'
        or $self->SUPER::declare($need, $names, @opts);

    foreach my $name (ref $names eq 'ARRAY' ? @$names : $names)
    {   # checking existence of opname is expensive here
        # and may be problematic with multiple bindings.
        $self->{XCW_dcopts}{$name} = $opts;
    }

    $self;
}

#--------------------------

=section Introspection

All of the following methods are usually NOT meant for end-users. End-users
should stick to the M<operation()> and M<compileClient()> methods.

=method index [$class, [$qname]]
With a $class and $qname, it returns one WSDL definition HASH or undef.
Returns the index for the $class group of names as HASH.  When no $class is
specified, a HASH of HASHes is returned with the CLASSes on the top-level.

$class includes C<service>, C<binding>, C<portType>, and C<message>.
=cut

sub index(;$$)
{   my $index = shift->{index};
    @_ or return $index;

    my $class = $index->{ (shift) }
       or return ();

    @_ ? $class->{ (shift) } : $class;
}

=method findDef $class, <$qname|$prefixed|$name>
With a $qname, the HASH which contains the parsed XML information
from the WSDL template for that $class-$name combination is returned.
You may also have a $prefixed name, using one of the predefined namespace
abbreviations.  Otherwise, $name is considered to be the localName in
that class.  When the $name is not found, an error is produced.

Without $qname in SCALAR context, there may only be one such name
defined otherwise an error is produced.  In LIST context, all definitions
in $class are returned.

=example
 $service  = $obj->findDef(service => 'http://xyz');
 @services = $obj->findDef('service');
=cut

sub findDef($;$)
{   my ($self, $class, $name) = @_;
    my $group = $self->index($class)
        or error __x"no definitions for `{class}' found", class => $class;

    if(defined $name)
    {   return $group->{$name} if exists $group->{$name};  # QNAME

        if($name =~ m/\:/)                                 # PREFIXED
        {   my $qname = $self->findName($name);
            return $group->{$qname} if exists $group->{$qname};
        }

        if(my $q = first { (unpack_type $_)[1] eq $name } keys %$group)
        {   return $group->{$q};
        }

        error __x"no definition for `{name}' as {class}, pick from:{groups}"
          , name => $name, class => $class
          , groups => join("\n    ", '', sort keys %$group);
    }

    return values %$group
        if wantarray;

    return (values %$group)[0]
        if keys %$group==1;
    my @alts = map $self->prefixed($_), sort keys %$group;
    error __x"explicit selection required: pick one {class} from {alts}"
      , class => $class, alts => join("\n    ", '', @alts);
}

=method operations %options
Return a list with all operations defined in the WSDL.

=option  service NAME
=default service <undef>
Only return operations related to the NAMEd service, by default all services.

=option  port NAME
=default port <undef>
Return only operations related to the specified port NAME.
By default operations from all ports.

=option  binding NAME
=default binding <undef>
Only return operations which use the binding with the specified NAME.
By default, all bindings are accepted.

=cut

sub operations(@)
{   my ($self, %args) = @_;
    $args{produce} and die "produce option removed in 0.81";

    my @ops;
    my @services = $self->findDef('service');
    foreach my $service (@services)
    {
        next if $args{service} && $args{service} ne $service->{name};

        my @ports = @{$service->{wsdl_port} || []};
        foreach my $port (@ports)
        {
            next if $args{port} && $args{port} ne $port->{name};
            my $bindtype = $port->{binding}
                or error __x"no binding defined in port '{name}'"
                      , name => $port->{name};
            my $binding  = $self->findDef(binding => $bindtype);

            next if $args{binding} && $args{binding} ne $binding->{name};

            my $type     = $binding->{type}
                or error __x"no type defined with binding `{name}'"
                    , name => $bindtype;

            my %all_ops;
            foreach my $operation ( @{$binding->{wsdl_operation}||[]} )
            {   my $name = $operation->{name};
                if($all_ops{$name}++)
                {   panic __x"operation {name} found again; pick service from {services}"
                      , services => [map $_->{name}, @services], _join => ', '
                        if @services > 1 && !$args{service};
                    panic __x"need one set of operations, pick port from {ports}"
                       , ports => [ map $_->{name}, @ports ], _join => ', ';
                }
  
                push @ops, $self->operation
                  ( service   => $service->{name}
                  , port      => $port->{name}
                  , binding   => $bindtype
                  , operation => $name
                  , portType  => $type
                  );
            }
        }
    }

    @ops;
}

=method endPoint %options
[2.20] Returns the address of the server, as specified by the WSDL. When
there are no alternatives for service or port, you not not need to
specify those parameters.

The endpoint in the WSDL is often wrong.  All compile functions accept
the C<server> and C<endpoint> parameters to overrule the value.  With
C<server>, only the hostname:port is being replaced.  With C<endpoint>,
everything is replaced.

=option  service QNAME|PREFIXED
=default service <undef>

=option  port    NAME
=default port    <undef>

=example
 my $devel = URI->new($wsdl->endPoint);
 $devel->path('/sdk');
 my $call = $wsdl->compileCall($opname, endpoint => $devel);
=cut

sub endPoint(@)
{   my ($self, %args) = @_;
    my $service   = $self->findDef(service => delete $args{service});

    my $port;
    my @ports     = @{$service->{wsdl_port} || []};
    my @portnames = map {$_->{name}} @ports;
    if(my $portname = delete $args{port})
    {   $port = first {$_->{name} eq $portname} @ports;
        error __x"cannot find port `{portname}', pick from {ports}"
            , portname => $portname, ports => join("\n    ", '', @portnames)
           unless $port;
    }
    elsif(@ports==1)
    {   $port = shift @ports;
    }
    else
    {   error __x"specify port explicitly, pick from {portnames}"
            , portnames => join("\n    ", '', @portnames);
    }

    foreach my $k (keys %$port)
    {   return $port->{$k}{location} if $k =~ m/address$/;
    }

    ();
}

=method printIndex [$fh], %options
For available %options, see M<operations()>.  This method is useful to
understand the structure of your WSDL: it shows a nested list of
services, bindings, ports and portTypes.
=cut

sub printIndex(@)
{   my $self = shift;
    my $fh   = @_ % 2 ? shift : select;
    my @args = @_;

    my %tree;
    foreach my $op ($self->operations(@args))
    {   my $port = $op->version.' port '.$op->portName;
        my $bind = '(binding '.$op->bindingName.')';
        $tree{'service '.$op->serviceName}{"$port $bind"}{$op->name} = $_;
    }

    foreach my $service (sort keys %tree)
    {   $fh->print("$service\n");
        foreach my $port (sort keys %{$tree{$service}})
        {   $fh->print("    $port\n");
            foreach my $op (sort keys %{$tree{$service}{$port}})
            {   $fh->print("        $op\n");
            }
        }
    }
}

=method explain $operation, $format, $direction, %options
[2.13]
Produce templates (see M<XML::Compile::Schema::template()> which detail
the use of the $operation. Currently, only the C<PERL> template $format
is available.

The $direction of operation is either C<INPUT> (input for the server,
hence to be produced by the client), or C<OUTPUT> (from the server,
received by the client).

The actual work is done by M<XML::Compile::SOAP::Operation::explain()>. The
%options passed to that method include C<recurse> and C<skip_header>.

=example
  print $wsdl->explain('CheckStatus', PERL => 'INPUT');

  print $wsdl->explain('CheckStatus', PERL => 'OUTPUT'
     , recurse => 1                 # explain options
     , port    => 'Soap12PortName'  # operation options
     );

  foreach my $op ($wsdl->operations)
  {  print $op->explain($wsdl, PERL => 'INPUT');
  }
=cut

sub explain($$$@)
{   my ($self, $opname, $format, $direction, @opts) = @_;
    my $op = $self->operation($opname, @opts)
        or error __x"explain operation {name} not found", name => $opname;
    $op->explain($self, $format, $direction, @opts);
}

#--------------------------------

=chapter DETAILS

=section Initializing SOAP operations via WSDL

When you have a WSDL file, then SOAP is simple.  If there is no such file
at hand, then it is still possible to use SOAP.  See the DETAILS chapter
in M<XML::Compile::SOAP>.

The WSDL file contains operations which can be addressed by name.
In the WSDL file you need to find the name of the port to be used.
In most cases, the WSDL has only one service, one port, one binding,
and one portType and those names can therefore be omitted.  If there is
a choice, then you must explicitly select one.

 use XML::Compile::WSDL11 ();

 # once in your program
 my $wsdl   = XML::Compile::WSDL11->new('def.wsdl');

 # XML::Compile::Schema refuses to follow "include" and
 # "import" commands, so you need to invoke them explicitly.
 # $wsdl->addWSDL('file2.wsdl');            # optional
 # $wsdl->importDefinitions('schema1.xsd'); # optional

 # once for each of the defined operations
 my $call   = $wsdl->compileClient('GetStockPrice');

 # see XML::Compile::SOAP chapter DETAILS about call params
 my $answer = $call->(%request);

=cut

1;
