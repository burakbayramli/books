#!/bin/bash

# functions

function calc() { 
   echo " 
   if ( $1 >= 0.0 ) {
      $1^5*e(-$1)
   } else {
      0.0
   } " | bc -l
}

# files from arguments

[ $# -lt 1 ] && echo "To few arguments" && exit 2
inp=$1

if [ $# -gt 1 ] ; then
   out=$2
else
   out=${1}_out
fi

[ ! -f $inp ] && echo "Specified input file doesn't exist" && exit 1


# some storage and counters

declare -a data
declare -i i
declare -i j
declare -a v
i=0

# read input

lines=$(wc -l < $inp );
exec 3< $inp

while [ $i -lt $lines ] ; do
   read data[$i] 0<&3
   i=i+1
done

# Do something with the data, and output.


i=0

while [ $i -lt $lines ] ; do

   # split entries on whitespace
   j=0
   for elm in ${data[$i]} ; do
      v[$j]=$elm
      j=j+1
   done
   newy=$(calc ${v[1]})
   echo "${v[0]} $newy" >> $out
   i=i+1
done



