#!/bin/sh -x
./clean.sh
root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`
gcc -O3 -g -I$numpy \
    -I$root/include/python$ver \
    -I$scripting/src/C \
    -c gridloop.c -o gridloop.o
gcc -shared -o ext_gridloop.so gridloop.o 

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop); \
           print ext_gridloop.__doc__'
           