#!/ifi/ganglot/k00/inf3330/www_docs/packages/SunOS/bin/perl
use CGI qw/:standard/;
use CGI::QuickForm;
#use CGI::Debug;
#use Data::Dumper;


# default values of input parameters:
$m = 1.0; $b = 0.7; $c = 5.0; $func = "y"; $A = 5.0; 
$w = 2*3.14159; $y0 = 0.2; $tstop = 30.0; $dt = 0.05; 
$case = "tmp1";

# make list of hashes:
@fields = [
  { -LABEL => 'm',      -TYPE => 'textfield', -default => $m, },
  { -LABEL => 'b',      -TYPE => 'textfield', -default => $b, },
  { -LABEL => 'c',      -TYPE => 'textfield', -default => $c, },
  { -LABEL => 'func',   -TYPE => 'textfield', -default => $func,},
  { -LABEL => 'A',      -TYPE => 'textfield', -default => $A, },
  { -LABEL => 'w',      -TYPE => 'textfield', -default => $w, },
  { -LABEL => 'y0',     -TYPE => 'textfield', -default => $y0,},
  { -LABEL => 'tstop',  -TYPE => 'textfield', -default => $tstop,},
  { -LABEL => 'dt',     -TYPE => 'textfield', -default => $dt,},
];


show_form(
  -ACCEPT => \&on_valid_form,  # must be supplied
  -TITLE  => "Oscillator",
  -INTRO  => '<img src="../../gui/figs/simviz.xfig.gif" align="left">',
  -FIELDS => @fields,
  -BUTTONS => [ {-name => 'compute'}, ], # "submit" button(s)
);


sub on_valid_form {
    # always start with this to ensure that other print
    # statements are directed to the browser:
    print header, start_html;  

    $m = param('m');
    $b = param('b');
    $c = param('c');
    $func = param('func');
    $A = param('A');
    $w = param('w');
    $y0 = param('y0');
    $tstop = param('tstop');
    $dt = param('dt');

    # run simulator and create plot, using simviz1.py:

    # important details:
    #   correct path to simviz1.py
    $app = "../../intro/python/simviz1.py";
    #   the current directory must have write permissions
    #       for anyone (chmod a+rwx .)
    #   make sure that simviz1.py finds the oscillator code, i.e.,
    #   define absolute path to the oscillator code and add to PATH:
    $osc = '/ifi/ganglot/k00/inf3330/www_docs/scripting/SunOS/bin';
    $ENV{PATH} = join ":", ($ENV{PATH}, $osc);

    $cmd = sprintf("%s -m %g -b %g -c %g -func %s -A %g -w %g -y0 %g -tstop %g -dt %g -case %s", 
		   $app,$m,$b,$c,$func,$A,$w,$y0,$tstop,$dt,$case);
    print "running<PRE>$cmd</PRE>\n";
    system "$cmd";
    # make sure anyone has write permission (next call to
    # this script can then remove subdir $case)
    chmod 0777, $case;  
    # show PNG image:
    $imgfile = $case . "/$case.png";
    # make an arbitrary new filename to prevent that browsers 
    # may reload the image from a previous run:
    $random_number = int(rand 2000);
    $newimgfile = $case . "/tmp_$random_number.png" ;
    rename $imgfile, $newimgfile;
    print img {src=>$newimgfile,align=>'TOP'}, end_html;
}
