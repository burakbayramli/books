: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

# default values of input parameters:
$m = 1.0; $b = 0.7; $c = 5.0; $func = "y"; $A = 5.0; 
$w = 2*3.14159; $y0 = 0.2; $tstop = 30.0; $dt = 0.05; 
$case = "tmp1"; $screenplot = 1;

# read variables from the command line, one by one:
while (@ARGV) {
    $option = shift @ARGV;   # load cmd-line arg into $option
    if ($option eq "-m") { 
	$m = shift @ARGV;    # load next command-line arg
    }
    elsif ($option eq "-b")     { $b     = shift @ARGV; }
    elsif ($option eq "-c")     { $c     = shift @ARGV; }
    elsif ($option eq "-func")  { $func  = shift @ARGV; }
    elsif ($option eq "-A")     { $A     = shift @ARGV; }
    elsif ($option eq "-w")     { $w     = shift @ARGV; }
    elsif ($option eq "-y0")    { $y0    = shift @ARGV; }
    elsif ($option eq "-tstop") { $tstop = shift @ARGV; }
    elsif ($option eq "-dt")    { $dt    = shift @ARGV; }
    elsif ($option eq "-noscreenplot") { $screenplot = 0; }
    elsif ($option eq "-case")  { $case  = shift @ARGV; }
    else {
	die "$0: invalid option '$option'\n";
    }
}

# create a subdirectory with name equal to case and generate
# all files in this subdirectory:
$dir = $case;
use File::Path;    # contains the rmtree function
if (-d $dir) {     # does $dir exist?
    rmtree($dir);  # remove directory (old files)
}
mkdir($dir, 0755) or die "Could not create $dir; $!\n"; 
chdir($dir)       or die "Could not move to $dir; $!\n";

# make input file to the program:
open(F,">$case.i") or die "open error; $!\n";
print F "
        $m
        $b
        $c
        $func
        $A
        $w
        $y0
        $tstop
        $dt
";
close(F);

# run simulator:
$cmd = "oscillator < $case.i";  # command to run
$failure = system($cmd);
die "running the oscillator code failed\n" if $failure;

# make gnuplot script:
open(F, ">$case.gnuplot");
print F "
set title '$case: m=$m b=$b c=$c f(y)=$func A=$A w=$w y0=$y0 dt=$dt';
";
if ($screenplot) {
    print F "plot 'sim.dat' title 'y(t)' with lines;\n";
}
print F <<EOF; # print multiple lines using a "here document"
set size ratio 0.3 1.5, 1.0;  
# define the postscript output format:
set term postscript eps monochrome dashed 'Times-Roman' 28;
# output file containing the plot:
set output '$case.ps';
# basic plot command:
plot 'sim.dat' title 'y(t)' with lines;
# make a plot in PNG format as well:
set term png small;
set output '$case.png';
plot 'sim.dat' title 'y(t)' with lines;
EOF
close(F);
# make plot:
$cmd = "gnuplot -geometry 800x200 -persist $case.gnuplot";
$failure = system($cmd);
die "running gnuplot failed\n" if $failure;
