#!/bin/bash

function statistics {
  avg=0; n=0
  for i in $@; do
    avg=`echo $avg + $i | bc -l`
    n=`echo $n + 1 | bc -l`
  done
  avg=`echo $avg/$n | bc -l`

  max=$1; min=$1; shift;
  for i in $@; do
    if [ `echo "$i < $min" | bc -l` != 0 ]; then
      min=$i; fi
    if [ `echo "$i > $max" | bc -l` != 0 ]; then
      max=$i; fi
  done
  printf "%.3f %g %g\n" $avg $min $max
}

statistics 1.2 6 -998.1 1 0.1
# statistics returns a list of numbers
res=`statistics 1.2 6 -998.1 1 0.1`

for r in $res; do echo "result=$r"; done

echo "average, min and max = $res"
