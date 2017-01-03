#!/bin/sh -x
./clean.sh
root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`
gcc -O3 -g -I$numpy -I$root/include/python$ver -I. \
    -I$scripting/src/C \
    -c gridloop_C.c gridloop_wrap.c
gcc -shared -o ext_gridloop.so gridloop_C.o gridloop_wrap.o

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop); \
           print ext_gridloop.__doc__'
           