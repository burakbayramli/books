#!/bin/sh -x
compiler=gcc
options=-O3

$compiler $options -o oscillator oscillator.c -lf2c -lm

exit
# install the oscillator executable:
if [ ! -d $scripting/$MACHINE_TYPE/bin ]; then
  mkdirhier $scripting/$MACHINE_TYPE/bin
fi
mv -f oscillator $scripting/$MACHINE_TYPE/bin
