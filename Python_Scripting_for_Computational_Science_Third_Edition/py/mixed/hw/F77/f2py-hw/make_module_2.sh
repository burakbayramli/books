#!/bin/sh -x
./clean.sh

# The quick f2py commands in make_module_1.sh treat s in hw3_v1 as
# an input argument, so we should make a .pyf file and edit it:

f2py -m hw -h hw.pyf --overwrite-signature ../hw.f 
#f2py -m hw -h hw.pyf --overwrite-signature --no-lower ../hw.f 

# automatic editing (we overwrite hw.pyf since we can repeat the editing):

scitools subst 'real\*8\s*::\s*s' 'real*8, intent(out) :: s' hw.pyf
f2py -c --fcompiler=gfortran hw.pyf ../hw.f

# test:
python -c 'import hw; print hw.hw3_v1(1.0,-1.0); print hw.__doc__'

# note: hw3 has a Cf2py directive so it always works well