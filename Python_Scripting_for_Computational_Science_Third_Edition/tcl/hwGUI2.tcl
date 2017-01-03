#!/bin/sh
# execute the wish in the path \
  exec wish -f "$0" ${1+"$@"}

# Hello World GUI; binding return in textentry to computations

label .hwtext -text "Hello, World! The sine of";
set r 1.2;    # default
entry .r      -width 6 -relief sunken -textvariable r
label .eq     -text "equals"
label .s      -textvariable s -width 14
# can pack everything at once (if desired):
pack .hwtext .r .eq .s -side left

proc comp_s { } {  global r; global s; set s [ expr sin($r) ] }
bind .r <Return>  comp_s

