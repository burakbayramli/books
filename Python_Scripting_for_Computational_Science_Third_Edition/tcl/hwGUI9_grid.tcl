#!/bin/sh
# execute the wish in the path \
  exec wish -f "$0" ${1+"$@"}

# define a frame for all widgets:
set top [ frame .everything ]
pack $top -side top

# use grid to place widgets in 3x4 cells:
set font "times 18 bold"
label $top.text -text "Hello, World!" -font $font;
grid  $top.text -row 0 -column 0 -columnspan 4 -pady 20

label $top.intro  -text "The sine of "
set r 1.2;  # default
entry $top.r      -width 6 -relief sunken -textvariable r
button $top.eq    -text " equals" -command comp_s -relief flat
label $top.s      -textvariable s -width 14
grid  $top.intro -row 1 -column 0
grid  $top.r     -row 1 -column 1
grid  $top.eq    -row 1 -column 2
grid  $top.s     -row 1 -column 3

button $top.quit -text "Goodbye, GUI World!" -command exit \
       -background yellow -foreground blue
grid $top.quit -row 2 -column 0 -columnspan 4 -pady 5 -sticky ew

bind $top.r <Return>  comp_s
proc comp_s { } {  global r; global s; set s [ expr sin($r) ] }
bind . <q> exit







