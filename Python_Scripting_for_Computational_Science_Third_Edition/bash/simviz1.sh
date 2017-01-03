#!/bin/sh
# default values of input parameters:
pi=3.14159
m=1.0; b=0.7; c=5.0; func="y"; A=5.0; 
w=`echo 2*$pi | bc`
y0=0.2; tstop=30.0; dt=0.05; case="tmp1"
screenplot=1

# read variables from the command line, one by one:
while [ $# -gt 0 ]
do
    option=$1; # load command-line arg into option
    shift;       # eat currently first command-line arg
    case "$option" in
	-m)
	    m=$1; shift; ;;  # load next command-line arg
	-b)
	    b=$1; shift; ;;
        -c)
	    c=$1; shift; ;;
        -func)
	    func=$1; shift; ;;
        -A)
	    A=$1; shift; ;;
        -w)
	    w=$1; shift; ;;
        -y0)
	    y0=$1; shift; ;;
        -tstop)
	    tstop=$1; shift; ;;
        -dt)
	    dt=$1; shift; ;;
        -noscreenplot)
	    screenplot=0; ;;
	-case)
	    case=$1; shift; ;;
	*)
	    echo "$0: invalid option \"$option\""; exit ;;
    esac

# if-tests corresponding to the case statement above:
#    if [ "$option" == "-m" ]; then
#        m=$1; shift;  # load next command-line arg
#    elif [ "$option" == "-b" ]; then
#        b=$1; shift;
#    ...
#    elif [ "$option" == "-case" ]; then
#        case=$1; shift;
#    else  
#        echo "$0: invalid option \"$option\""; exit
#    fi
done

# create a subdirectory with name equal to case and generate
# all files in this subdirectory:
dir=$case
if [ -d $dir ]; then
  rm -r $dir
fi
mkdir $dir
cd $dir

# make input file to the program:
cat > $case.i <<EOF
        $m
        $b
        $c
        $func
        $A
        $w
        $y0
        $tstop
        $dt
EOF

# run simulator:
oscillator < $case.i
if [ "$?" != "0" ]; then
  echo "running oscillator failed"; exit 1
fi

# make gnuplot script:
echo "set title '$case: m=$m b=$b c=$c f(y)=$func A=$A w=$w y0=$y0 dt=$dt'" > $case.gnuplot
if [ "$screenplot" != "0" ]; then
  echo "plot 'sim.dat' title 'y(t)' with lines;" >> $case.gnuplot
fi
cat >> $case.gnuplot <<EOF
set size ratio 0.3 1.5, 1.0;  
# define the postscript output format:
set term postscript eps monochrome dashed 'Times-Roman' 28;
# output file containing the plot:
set output '$case.ps';
# basic plot command:
plot 'sim.dat' title 'y(t)' with lines;
# make a plot in PNG format as well:
set term png small color;
set output '$case.png';
plot 'sim.dat' title 'y(t)' with lines;
EOF
gnuplot -geometry 800x200 -persist $case.gnuplot
if [ "$?" != "0" ]; then
  echo "running gnuplot failed"; exit 1
fi
