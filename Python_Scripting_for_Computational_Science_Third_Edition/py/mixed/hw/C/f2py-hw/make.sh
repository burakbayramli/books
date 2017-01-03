#!/bin/sh -x
# use f2py to interface hw.c

# Step 1: write the .pyf file or use an F77 header 
# to automatically generate it
f2py -m hw -h hw.pyf --overwrite-signature only: hw1 hw2 hw3 : signatures.f
# Step 2: compile C code and make module:
f2py -c hw.pyf ../hw.c

python -c 'import hw; print hw.hw3(1.0,-1.0); print hw.__doc__'

