#!/bin/sh
# execute the tclsh in the path \
  exec tclsh "$0" ${1+"$@"}

set infilename ".myprog.cpp"
set INFILE [open $infilename r] ;  # open for reading
set filestr [read $INFILE];        # load file into a string
set inlines [split $filestr "\n"]; # split string into list of lines
# alternative reading, line by line:
while {[gets $INFILE line] >= 0} {
  # process $line
}
close $INFILE

set outfilename ".myprog2.cpp"
set OUTFILE [open $outfilename w]; # open for writing
set line_no 0;  # count the line number in the output file
foreach line $inlines {
   incr line_no
   puts $OUTFILE "$line_no: $line"
}
close $OUTFILE

set filename $outfilename
set OUTFILE [open $filename a];  # open for appending
# multi-line output (like a "here document"):
puts $OUTFILE \
"/*
  This file, \"$outfilename\", is a version
  of \"$infilename\" where each line is numbered.
*/"
# puts -nonewline avoids automatic appending of \n
close $OUTFILE

#-------------------------------------------------------------
set PIPE [open "| sh" w]
puts $PIPE "echo Hello Pipe World"
close $PIPE

#-------------------------------------------------------------

set myarg1 "myfile.1"

# make a list of words:
set arglist [list $myarg1 "displacement" tmp.ps]

# extract elements form a list:
set filename  [lindex $arglist 0];  # first (0) entry
set plottitle [lindex $arglist 1]
set ppsfile   [lindex $arglist 2]

# push an item onto a list:
set myvar2 myvar2
lappend arglist $myvar2

# traverse a list:
foreach entry $arglist { 
  puts "entry is $entry" 
}


# -------------------------------------------------------

set line1 "iteration 12:    eps= 1.245E-05"
set words1 [split $line1 " "] 
# words1 entry 0 is "iteration"
# words1 entry 1 is "12:"
# words1 entry 2 is "eps="
# words1 entry 3 is "1.245E-05"
set i 0; foreach word $words1 {
  puts -nonewline [format "words1\[%d\]=\"%s\"\n" $i \
                   [lindex $words1 $i ]]
  incr i
}

# alternative output:
for {set i 0} {$i < [llength $words1]} {incr i} {
  puts -nonewline [format "words1\[%d\]=\"%s\"\n" $i \
                   [lindex $words1 $i ]]
}

set sorted_words [lsort $words1]
puts "words1=$words1, sorted_words=$sorted_words"

set newline1 [join $words1 "#"]
# newline1 is "iteration 12:#eps#1.245E-05"
puts "newline1 is now \[$newline1\]"

set line2 ".myc_12@displacement@u(x,3.1415)@  no upwinding"

set words2 [split $line2 "@"]
# words2 entry 0 is ".myc_12"
# words2 entry 1 is "displacement"
# words2 entry 2 is "u(x,3.1415)"
# words2 entry 3 is "  no upwinding"
set i 0; foreach word $words2 {
   puts -nonewline [format "words(%d)=%s\n" $i [lindex $words2 $i]] 
   incr i 1
}

# --------------------------------------------------------------

# substitute Hello by Hi everywhere in a file:
set filename "hw.tcl"
set FILE [open $filename r]
set lines [read $FILE]
close $FILE
# no need to join lines in Tcl, they are joined
set line $lines
puts "lines on file:\n$line"
regsub -all Hello $line Hi line2;  # substitute
set line $line2
puts "regex sub:\n$line"

# ---------------------------------------------

set myfile hw.tcl
if [ file isfile      $myfile ] { puts "$myfile is a file"      }
if [ file isdirectory $myfile ] { puts "$myfile is a directory" }
if [ file executable  $myfile ] { puts "$myfile is executable"  }

file stat $myfile statvar
# convert time values to human-readable form:
set atime [clock format $statvar(atime)]
set ctime [clock format $statvar(atime)]
set mtime [clock format $statvar(atime)]
puts "$myfile has atime=$atime, ctime=$ctime \
     \nmtime=$mtime), size=$statvar(size) bytes"

# -----------------------------------------------------------------

set thisdir [pwd]
set dir mynewdir
if [ file isdirectory $dir] {} else {
   puts "$dir did not exist, I'll make one..."
   file mkdir $dir
   cd $dir
}
cd;  # move to home directory
puts "current dir for commontasks.tcl is [pwd]"
cd $thisdir
puts "current dir for commontasks.tcl is [pwd]"

exec rm -r $dir

# -------------------------------------------------------
set program  "vtk"
set found  0
set path $env(PATH);  # e.g. /usr/bin:/usr/local/bin:/usr/X11/bin
set paths [split $path ":"]
foreach dir $paths {
  if [file executable $dir] {
      puts "checking directory $dir"
      if [file isfile "$dir/$program"] {
         set found 1; set path $dir; break; }
  }
}
if $found { puts "$program found in $path" 
} else { puts "$program: not found" }

# -------------------------------------------------------

set filelist [glob *.tcl];
foreach file $filelist { puts "Tcl file: $file" }

# -------------------------------------------------------

set tmpfile tmp.tcl
file copy -force hw.tcl $tmpfile
file rename -force $tmpfile tmp2.tcl
file copy -force hw.tcl tmp2.tcl
file copy -force hw.tcl tmp3.tcl
file delete -force [glob tmp?.tcl]
file delete -force tmp.tcl

# file mkdir creates the whole path:
file mkdir "$env(HOME)/perl/projects/test1"
if { [file isdirectory "$env(HOME)/perl/projects/test1"] } {
  puts "\nYes! \"$env(HOME)/perl/projects/test1\" was created!" 
}
file delete -force "$env(HOME)/perl"
set name "/usr/home/hpl/scripting/perl/intro/hw2.pl"
set basename [file tail $name]
set dirname [file dirname $name]
puts "\n   $name\nhas basename $basename and dirname $dirname"

# -------------------------------------------------------

# hashes:
set cmlargs(-tstop) 6.0
set cmlargs(-c_in_H) 1.2
# or
# initialize a hash (array) in a one-line operation:
array set cmlargs [list -tstop 6.0 -c_in_H 1.2]

# initialize a list of (simulated) command-line arguments:
set myarg [ list "-myopt" 1.2 "-c_in_H" 9.8 "-tstop" 6.1 ]

# run through each command-line argument:
set arg_counter 0
set nmyargs [llength $myarg]
while { $arg_counter < $nmyargs } {
   set option [lindex $myarg $arg_counter ]
   if { [string length [array names cmlargs $option]] > 0 } {
      incr arg_counter 
      set value [lindex $myarg $arg_counter ] 
      set cmlargs($option) $value 
      puts "associative array: cmlargs, key=$option value=$value" 
   } else {
       puts "The option $option is not registered";
   }
   incr arg_counter
}
# print the array:
foreach item [ array names cmlargs ] {
   puts "cmlargs($item)=$cmlargs($item)"
}

# procedures:

proc statistics { args } {
   set avg 0; set n 0
   foreach term $args { incr n; set avg [expr $avg + $term] }
   set avg [expr $avg/$n]
   set min [lindex $args 1]; set max $min
   foreach term $args { 
       if { $term < $min } { set min $term }
       if { $term > $max } { set max $term }
   }
   set r [list $avg $min $max]
   puts "statistics: return $r"
   return $r
}

set v1 1.1; set v2 5.8; set v3 9; set b 1;

#statistics $v1 $v2 $v3 $b

set r [ statistics $v1 $v2 $v3 $b ]
puts "\n\nstatistics: avg=[lindex $r 0], min=[lindex $r 1], max=[lindex $r 2]"

set v1 1.3; set v2 "some text"

proc swap { a b } {
    upvar $a a1
    upvar $b b1
    set tmp $a1
    set a1 $b1
    set b1 $tmp
}

puts "before swap: v1=\[$v1\] v2=\[$v2\]"
swap v1 v2;  # swap the values of $v1 and $v2
# notice that swap $v1 $v2 does not work here, must send the variables
puts "after  swap: v1=\[$v1\] v2=\[$v2\]"

proc print2file { { message "no message" } { file "tmp.tmp" } } {
  puts "print2file: message=$message, file=$file"
}

print2file "bla-bla" "mylongfilenamefile.txt"
print2file

# named arguments:
proc displaylist { args } {
    # store arguments in an array (hash) options
    # set default values of arguments:
    array set options [list "-list" 0 "-help" 0 "-label" "  " ]
    # traverse the args list, extract option names and values:
    set arg_counter 0
    set nargs [llength $args]
    while { $arg_counter < $nargs } {
	set name  [lindex $args $arg_counter ]
	incr arg_counter
	set value [lindex $args $arg_counter ]
        # is name an option we know of?
        foreach option [ array names options ] {
            if {[ string compare $name $option ] == 0} {
		set options($name) $value
	    }
	}
	# ready for next item in args:
	incr arg_counter
    }
    foreach option [ array names options ] {
	puts "options($option)=$options($option)"
    }
    puts "\n$options(-label):\n";
    set index 0
    foreach item $options(-list) {
	set helptext [lindex $options(-help) $index]
        puts [format "item %d: %-10s description: %s" \
                     $index $item $helptext]
	incr index
    }
}

set curves [list curve1 curve2 curve3]
set explanations [list "curve1: initial shape of u" \
                       "initial shape of H" \
                       "shape of u at time=2.5"]
set leading_text "pretty print of two lists"

displaylist -list $curves -help $explanations -label $leading_text


# --------------------------------------------------------------------

#class mybase {
#    variable i
#    variable j
#
#    constructor { i_ j_ } {
#	set i $i_
#	set j $j_
#    }
#
#    write { } {
#	puts "mybase: i=$i, j=$j"
#    }
#}

#class mysub {
#    inherit mybase
#    variable k

#    constructor { k_ } {
#	set k $k_
#    }

#    write { } { }
#}
