#!/bin/sh -x

# compile the F77 wave simulator

# first transform F77WAVE.fcp, written with the aid of the
# C preprocessor, to plain F77:
./fcpp.py F77WAVE.fcp

# compile and link:
gfortran -O -o app -pg F77WAVE.f main.f
