#!/bin/sh -x
# compile F77 code and make an executable tmp.app
# (use GNU gfortran)
gfortran -o tmp.app -O hw.f

# test:
./tmp.app
