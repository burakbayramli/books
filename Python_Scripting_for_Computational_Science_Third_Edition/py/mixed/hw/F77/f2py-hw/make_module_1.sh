#!/bin/sh -x
# compile F77 code and make a shared library module for use with Python
./clean.sh

# we create the module in three ways: 
#   single-command (quick and easy) f2py 
#   two-step f2py involving editing of .pyf file
#   single-command f2py on Fortran source with in/out variable spec.

# simplest:
#f2py -m hw -c ../hw.f

# specify compiler explicitly:
f2py -m hw -c --fcompiler=gfortran ../hw.f

# test:
python -c 'import hw; print hw.__doc__'

# observe that hw3_v1 is wrong:
python -c 'from hw import hw3_v1; \
           r1=1; r2=-1; s=10; \
           hw3_v1(r1,r2,s); \
           print "s should be 0 but is not... s =",s; \
           import hw; \
           print hw.__doc__; \
           print hw.hw3_v1.__doc__'


