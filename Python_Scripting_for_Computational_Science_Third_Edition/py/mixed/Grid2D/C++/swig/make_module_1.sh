#!/bin/sh -x
./clean.sh

root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`

swig -python -c++ -I. -I../plain -I$numpy ext_gridloop.i

g++ -I. -O3 -g -I../plain -I$numpy -I$root/include/python$ver \
    -c ../plain/NumPyArray.cpp gridloop.cpp ext_gridloop_wrap.cxx
g++ -shared -o _ext_gridloop.so NumPyArray.o \
    gridloop.o ext_gridloop_wrap.o

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop);'
           