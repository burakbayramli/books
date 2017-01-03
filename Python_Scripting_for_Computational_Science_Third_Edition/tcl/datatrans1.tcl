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
set OUTFILE [open $outfilename w];  # open for writing

proc myfunc { y } {
    if {$y >= 0.0} { return [expr pow($y,5.0)*exp(-$y)] }
    else           { return 0.0; }
}

# read one line at a time:
while {[gets $INFILE line] >=0} {
    # replace multiple whitespace chars by " " to avoid
    # empty strings when splitting the line into words;
    # remove leading and trailing white space:
    set line [string trim $line]
    # replace one or more white spaces (regex \s+) by " ":
    regsub -all {\s+} $line " " line2
    # split line2 into a list of words (x and y coord.):
    set xy [split $line2 " "]; # xy is a list of words in line2
    set xi [lindex $xy 0];  set yi [lindex $xy 1]
    set fy [myfunc $yi];  # transform y value 
    puts $OUTFILE [format "%g  %12.5e" $xi $fy]
}
close $INFILE; close $OUTFILE

