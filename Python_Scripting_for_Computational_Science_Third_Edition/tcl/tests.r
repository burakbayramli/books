./tests.verify: test performed on 2004.08.14 


#### Test: ./tests.verify running hw.tcl 1.2
Hello, World! sin(1.2)=0.932039085967
CPU time of hw.tcl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running datatrans1.tcl ../py/intro/.datatrans_infile tmp.1
CPU time of datatrans1.tcl: 0.0 seconds on hplx30 i686, Linux



----- appending file tmp.1 ------
0.1   5.36092e-01
0.2   3.12343e+00
0.3   5.87269e+00
0.4   3.12343e+00

#### Test: ./tests.verify running datatrans2.tcl ../py/intro/.datatrans_infile tmp.1
CPU time of datatrans2.tcl: 0.0 seconds on hplx30 i686, Linux



----- appending file tmp.1 ------
0.1   5.36092e-01
0.2   3.12343e+00
0.3   5.87269e+00
0.4   3.12343e+00

#### Test: ./tests.verify running commontasks.tcl 
Hello Pipe World
entry is myfile.1
entry is displacement
entry is tmp.ps
entry is myvar2
words1[0]="iteration"
words1[1]="12:"
words1[2]=""
words1[3]=""
words1[4]=""
words1[5]="eps="
words1[6]="1.245E-05"
words1[0]="iteration"
words1[1]="12:"
words1[2]=""
words1[3]=""
words1[4]=""
words1[5]="eps="
words1[6]="1.245E-05"
words1=iteration 12: {} {} {} eps= 1.245E-05, sorted_words={} {} {} 1.245E-05 12: eps= iteration
newline1 is now [iteration#12:####eps=#1.245E-05]
words(0)=.myc_12
words(1)=displacement
words(2)=u(x,3.1415)
words(3)=  no upwinding
lines on file:
#!/usr/bin/tclsh
set r [lindex $argv 0]; # load first command-line argument
set s [expr sin($r)];   # store sin(r) in s
puts "Hello, World! sin($r)=$s"


regex sub:
#!/usr/bin/tclsh
set r [lindex $argv 0]; # load first command-line argument
set s [expr sin($r)];   # store sin(r) in s
puts "Hi, World! sin($r)=$s"


hw.tcl is a file
hw.tcl is executable
hw.tcl has atime=Sat Aug 14 08:37:53 CEST 2004, ctime=Sat Aug 14 08:37:53 CEST 2004  
mtime=Sat Aug 14 08:37:53 CEST 2004), size=153 bytes
mynewdir did not exist, I'll make one...
current dir for commontasks.tcl is /home/hpl
current dir for commontasks.tcl is /home/work/scripting/src/tcl
checking directory /home/hpl/bin
checking directory /work/scripting/src/tools
checking directory /work/scripting/Linux/bin
checking directory /bin
checking directory /usr/bin
checking directory /usr/X11R6/bin
checking directory /usr/sbin
checking directory /home/hpl/install/bin
checking directory /home/hpl/hp/pyPDE/src/lib
checking directory /home/hpl/hp/pyPDE/src/bin
checking directory /work/NO/bin
checking directory /work/NO/md/bin/Linux/opt
checking directory /work/NO/dp/bin/Linux/opt
checking directory /work/NO/bt/bin/Linux/opt
checking directory /work/sysdir/Linux/bin
checking directory /work/NO/internal/bin
checking directory /usr/local/Acrobat5/bin
checking directory /work/sysdir/src/python/Jython/jython-2.0
checking directory .
vtk: not found
Tcl file: simviz1.tcl
Tcl file: commontasks.tcl
Tcl file: hwGUI7_novar.tcl
Tcl file: datatrans1.tcl
Tcl file: datatrans2.tcl
Tcl file: demoGUI.tcl
Tcl file: findsize.tcl
Tcl file: hw.tcl
Tcl file: hwGUI9_grid.tcl
Tcl file: hwGUI1.tcl
Tcl file: hwGUI2.tcl
Tcl file: hwGUI3.tcl
Tcl file: hwGUI4.tcl
Tcl file: hwGUI5.tcl
Tcl file: hwGUI6.tcl
Tcl file: hwGUI7.tcl
Tcl file: hwGUI8.tcl
Tcl file: hwGUI9.tcl

Yes! "/home/hpl/perl/projects/test1" was created!

   /usr/home/hpl/scripting/perl/intro/hw2.pl
has basename hw2.pl and dirname /usr/home/hpl/scripting/perl/intro
The option -myopt is not registered
The option 1.2 is not registered
associative array: cmlargs, key=-c_in_H value=9.8
associative array: cmlargs, key=-tstop value=6.1
cmlargs(-c_in_H)=9.8
cmlargs(-tstop)=6.1
statistics: return 4.225 1 9


statistics: avg=4.225, min=1, max=9
before swap: v1=[1.3] v2=[some text]
after  swap: v1=[some text] v2=[1.3]
print2file: message=bla-bla, file=mylongfilenamefile.txt
print2file: message=no message, file=tmp.tmp
options(-label)=pretty print of two lists
options(-list)=curve1 curve2 curve3
options(-help)={curve1: initial shape of u} {initial shape of H} {shape of u at time=2.5}

pretty print of two lists:

item 0: curve1     description: curve1: initial shape of u
item 1: curve2     description: initial shape of H
item 2: curve3     description: shape of u at time=2.5
CPU time of commontasks.tcl: 0.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running simviz1.tcl -A 5.0 -tstop 2 -case tmp4
CPU time of simviz1.tcl: 0.1 seconds on hplx30 i686, Linux



----- appending file tmp4/tmp4.i ------

        1.0
        0.7
        5.0
        y
        5.0
        6.28318
        0.2
        2
        0.05



----- appending file tmp4/tmp4.gnuplot ------

set title 'tmp4: m=1.0 b=0.7 c=5.0 f(y)=y A=5.0 w=6.28318 y0=0.2 dt=0.05';
set size ratio 0.3 1.5, 1.0;  
# define the postscript output format:
set term postscript eps monochrome dashed 'Times-Roman' 28;
# output file containing the plot:
set output 'tmp4.ps';
# basic plot command:
plot 'sim.dat' title 'y(t)' with lines;
# make a plot in PNG format as well:
set term png small color;
set output 'tmp4.png';
plot 'sim.dat' title 'y(t)' with lines;



----- appending file tmp4/tmp4.ps (just 30 lines) ------
%!PS-Adobe-2.0 EPSF-2.0
%%Title: tmp4.ps
%%Creator: gnuplot 3.7 patchlevel 3
%%CreationDate: Sat Aug 14 08:37:53 2004
%%DocumentFonts: (atend)
%%BoundingBox: 50 50 590 302
%%Orientation: Portrait
%%EndComments
/gnudict 256 dict def
gnudict begin
/Color false def
/Solid false def
/gnulinewidth 5.000 def
/userlinewidth gnulinewidth def
/vshift -93 def
/dl {10 mul} def
/hpt_ 31.5 def
/vpt_ 31.5 def
/hpt hpt_ def
/vpt vpt_ def
/M {moveto} bind def
/L {lineto} bind def
/R {rmoveto} bind def
/V {rlineto} bind def
/vpt2 vpt 2 mul def
/hpt2 hpt 2 mul def
/Lshow { currentpoint stroke M
  0 vshift R show } def
/Rshow { currentpoint stroke M
  dup stringwidth pop neg vshift R show } def
