#!/usr/bin/tclsh
set r [lindex $argv 0]; # load first command-line argument
set s [expr sin($r)];   # store sin(r) in s
puts "Hello, World! sin($r)=$s"

