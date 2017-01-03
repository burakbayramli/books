#!/bin/sh
r="$1"  # store first command-line argument in r
# call up perl to perform math:
s=`perl -e '$s=sin($ARGV[0]); print $s;' $r`
echo "Hello, World! sin($r)=$s"  # print to the screen
