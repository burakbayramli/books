#!/bin/sh
# execute the tclsh in the path \
  exec tclsh "$0" ${1+"$@"}

# default values of input parameters:
set m 1.0; set b 0.7; set c 5.0; set func y; set A 5.0
set w [expr 2*3.14159]; set y0 0.2; set tstop 30.0
set dt 0.05; set case tmp1

# read variables from the command line, one by one:
while { [llength $argv] > 1 } {
    set option [lindex $argv 0]
    if { [string compare $option "-m"] == 0 } {
	# remove 1st command-line arg.:
	set argv [lreplace $argv 0 0]; 
	# assign (the new) 1st command-line arg. to m:
	set m [lindex $argv 0]
    } \
    elseif { [string compare $option "-b"] == 0 } {
	set argv [lreplace $argv 0 0]
	set b [lindex $argv 0]
    } \
    elseif { [string compare $option "-c"] == 0 } {
	set argv [lreplace $argv 0 0]
	set c [lindex $argv 0]
    } \
    elseif { [string compare $option "-func"] == 0 } {
	set argv [lreplace $argv 0 0]
	set func [lindex $argv 0]
    } \
    elseif { [string compare $option "-A"] == 0 } {
	set argv [lreplace $argv 0 0]
	set A [lindex $argv 0]
    } \
    elseif { [string compare $option "-w"] == 0 } {
	set argv [lreplace $argv 0 0]
	set w [lindex $argv 0]
    } \
    elseif { [string compare $option "-y0"] == 0 } {
	set argv [lreplace $argv 0 0]
	set y0 [lindex $argv 0]
    } \
    elseif { [string compare $option "-tstop"] == 0 } {
	set argv [lreplace $argv 0 0]
	set tstop [lindex $argv 0]
    } \
    elseif { [string compare $option "-dt"] == 0 } {
	set argv [lreplace $argv 0 0]
	set dt [lindex $argv 0]
    } \
    elseif { [string compare $option "-case"] == 0 } {
	set argv [lreplace $argv 0 0]
	set case [lindex $argv 0]
    } \
    else { puts "$argv0: invalid option \"$option\""; exit 1 }
    set argv [lreplace $argv 0 0];  # remove the option's value
}

# change current working directory:
set dir $case
if { [file isdirectory $dir] != 0 } { 
    file delete -force $dir 
}
file mkdir $dir
cd $dir

# make input file to the program:
set F [open "$case.i" w]
# write multi-line string with variable substitution:
puts $F "
        $m
        $b
        $c
        $func
        $A
        $w
        $y0
        $tstop
        $dt
"
close $F

# run simulator:
set cmd "oscillator < $case.i";  # command to run
# this one works: eval exec $cmd >@stdout
exec sh -c $cmd >@stdout;  # safe, but tied to Unix

# make gnuplot script:
set F [open "$case.gnuplot" w]
# write multi-line string with variable substitution:
puts $F "
set title '$case: m=$m b=$b c=$c f(y)=$func A=$A w=$w y0=$y0 dt=$dt';
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
"
close $F

# make plot:
set cmd "gnuplot $case.gnuplot";  # command to run
exec sh -c $cmd >@stdout
