#!/bin/sh -x
compiler=gfortran
options="-O3 -std=legacy"
$compiler -o oscillator $options oscillator.f

# install the oscillator executable:
if [ ! -d $scripting/$MACHINE_TYPE/bin ]; then
  mkdir -p $scripting/$MACHINE_TYPE/bin
fi
mv -f oscillator $scripting/$MACHINE_TYPE/bin


