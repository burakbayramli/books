#!/bin/sh
# execute the wish in the path \
  exec wish -f "$0" ${1+"$@"}

label .hwtext -text "Hello, World! The sine of"
pack  .hwtext -side left

set r 1.2;  # default
entry .r -width 6 -relief sunken -textvariable r
pack  .r -side left

button .compute -text " equals " -command  { comp_s }
pack   .compute -side left
proc comp_s { } {  global r; global s; set s [ expr sin($r) ] }

label .s -textvariable s -width 14
pack  .s -side left
