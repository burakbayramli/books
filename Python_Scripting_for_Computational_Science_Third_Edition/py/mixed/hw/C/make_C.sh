#!/bin/sh -x
# compile C code and make an executable tmp.app
# (use GNU gcc)
gcc -o tmp.app -O hw.c -lm

# test:
./tmp.app 1.0 0

