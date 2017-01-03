#!/bin/sh -x
# compile F90 code and make a shared library module for use with Python
./clean.sh

# specify compiler explicitly:
f2py -m hw -c --fcompiler=gfortran ../hw.f90

# test:
python -c 'import hw; print hw.__doc__'

python hwa.py -1.2 1.2


