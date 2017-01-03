#!/bin/sh -x
# compile F90 code and make an executable tmp.app
# (use GNU gfortran)
gfortran -o tmp.app -O hw.f90

# test:
./tmp.app
