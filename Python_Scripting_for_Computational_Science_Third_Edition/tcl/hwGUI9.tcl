#!/bin/sh
# execute the wish in the path \
  exec wish -f "$0" ${1+"$@"}

# define a frame for all subframes:
set top [ frame .everything ]
pack $top -side top

# first frame consists of the Hello World text:
set font "times 18 bold"
set hw [ frame $top.hw ]
pack $hw -side top
label $hw.text -text "Hello, World!" -font $font;
pack $hw.text -side top -pady 20

# second frame consists of the sine computations:
set sine [ frame $top.sine ]
pack $sine -side top -pady 20 -padx 10
label $sine.intro  -text "The sine of "
set r 1.2;  # default
entry $sine.r      -width 6 -relief sunken -textvariable r
button $sine.eq    -text " equals" -command comp_s -relief flat
label $sine.s      -textvariable s -width 14
pack $sine.intro $sine.r $sine.eq $sine.s -side left

# third frame consists of a quit button (we drop the
# frame and add the button straight into the toplevel widget)
button $top.quit -text "Goodbye, GUI World!" -command exit \
       -background yellow -foreground blue
pack $top.quit -side top -pady 5 -fill x

bind $sine.r <Return>  comp_s
proc comp_s { } {  global r; global s; set s [ expr sin($r) ] }
bind . <q> exit
