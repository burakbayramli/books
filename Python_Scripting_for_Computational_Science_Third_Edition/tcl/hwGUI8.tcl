#!/bin/sh
# execute the wish in the path \
  exec wish -f "$0" ${1+"$@"}

# first frame consists of the Hello World text:
set font "times 18 bold"
frame .hw
label .hw.text -text "Hello, World!" -font $font;
pack .hw.text -side top -pady 20
pack .hw -side top -anchor w

# second frame consists of the sine computations:
frame .sine 
pack  .sine -side top -pady 20 -padx 10 -anchor w
label .sine.intro  -text "The sine of "
set r 1.2;  # default
entry .sine.r      -width 6 -relief sunken -textvariable r
label .sine.eq     -text " equals"
label .sine.s      -textvariable s -width 14
pack  .sine.intro .sine.r .sine.eq .sine.s -side left

# third frame consists of a quit button (we drop the
# frame and add the button straight into the toplevel widget)
button .quit -text "Goodbye, GUI World!" -command exit \
       -background yellow -foreground blue
pack .quit -side top -pady 5 -anchor w -ipadx 30 -ipady 30

bind .sine.r <Return>  comp_s
proc comp_s { } {  global r; global s; set s [ expr sin($r) ] }
bind . <q> exit
