use ExtUtils::MakeMaker;

use 5.008;

my $version = '3.00';

my $prereq =
   { XML::Compile         => 1.37
   , XML::Compile::SOAP11 => '3.00'
   , XML::Compile::Cache  => 0.995
   , Log::Report          => '1.00'
   , Test::More           => 0.54
   };

WriteMakefile
 ( NAME	 	=> 'XML::Compile::WSDL11'
 , VERSION	=> $version
 , PREREQ_PM    => $prereq
 , AUTHOR       => 'Mark Overmeer'
 , ABSTRACT     => 'WSDL version 1.1'
 , LICENSE      => 'perl'
 );

### used by oodist during production of distribution
sub MY::postamble { <<'__POSTAMBLE' }

# for DIST
RAWDIR          = ../public_html/xml-compile-wsdl11/raw
DISTDIR         = ../public_html/xml-compile-wsdl11/source
LICENSE         = artistic
SKIP_LINKS	= XML::LibXML

# for POD
FIRST_YEAR      = 2014
EMAIL           = perl@overmeer.net
WEBSITE         = http://perl.overmeer.net/xml-compile/
PODTAIL		= ../XML-shared-podtail
__POSTAMBLE
