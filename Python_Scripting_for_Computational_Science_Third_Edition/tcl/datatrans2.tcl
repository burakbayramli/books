#!/bin/sh
# execute the tclsh in the path \
  exec tclsh "$0" ${1+"$@"}

# issue error message if not two arguments are given:
if {$argc < 2} { 
    puts "Usage: $argv0 infilename outfilename ..." 
    exit 1 
}

set infilename  [lindex $argv 0]
set outfilename [lindex $argv 1]

set INFILE  [open $infilename  r];  # open for reading
set filestr [read $INFILE]; # read file into a string
set lines [split $filestr "\n"]; # split string wrt. newline
close $INFILE

# go through each line and split line into x and y columns:
foreach line $lines {
    # replace multiple whitespace chars by " " to avoid
    # empty strings when splitting the line into words;
    # remove leading and trailing white space:
    set line [string trim $line]
    # replace one or more white spaces (regex \s+) by " ":
    regsub -all {\s+} $line " " line2
    # split line2 into a list of words:
    set xy [split $line2 " "]
    if {[llength $xy] == 2} {
	# add coordinates to their respective lists (x and y):
	lappend x [lindex $xy 0]
	lappend y [lindex $xy 1]
    }
    # else: newline at the end gives extra blank line in $lines
}    

proc myfunc { y } {
    if {$y >= 0.0} { return [expr pow($y,5.0)*exp(-$y)] } \
    else           { return 0.0; }
}

set OUTFILE [open $outfilename w];  # open for writing
set npairs [llength $x]
for {set i 0} {$i < $npairs} {incr i} {
    set fy [myfunc [lindex $y $i]]
    puts $OUTFILE [format "%g  %12.5e" [lindex $x $i] $fy]
}
close $OUTFILE

