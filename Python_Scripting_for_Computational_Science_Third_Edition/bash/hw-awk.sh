#!/bin/sh
# as hw1.sh, but calling awk (instead of Perl) to compute the sine:
r=$1  # store first command-line argument in r
s=`awk "BEGIN { s=sin($r); print s;}" hw1.sh`
echo "Hello, World! sin($r)=$s"  # print to the screen
