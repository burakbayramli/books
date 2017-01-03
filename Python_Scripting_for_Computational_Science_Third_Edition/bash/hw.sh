#!/bin/sh
r=$1  # store first command-line argument in r
s=`echo "s($r)" | bc -l`
echo "Hello, World! sin($r)=$s"  # print to the screen
