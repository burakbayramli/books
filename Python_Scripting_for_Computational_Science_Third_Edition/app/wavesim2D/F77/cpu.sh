#!/bin/sh
# simple script for extracting the CPU time from gprof output:
gprof app | perl -ne 'if (/MAIN__$/) 
                      { $cpu=(split /\s+/)[2]; print "CPU=$cpu\n"; }'
