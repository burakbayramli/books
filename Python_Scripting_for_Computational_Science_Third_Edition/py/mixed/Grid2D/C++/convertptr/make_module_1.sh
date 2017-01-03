#!/bin/sh -x
./clean.sh

root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`

swig -python -c++ -I. -I$numpy ext_gridloop.i

g++ -I. -O3 -g -I$root/include/python$ver -I$numpy \
    -c convert.cpp gridloop.cpp ext_gridloop_wrap.cxx
g++ -shared -o _ext_gridloop.so \
     convert.o gridloop.o ext_gridloop_wrap.o

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop);'
           