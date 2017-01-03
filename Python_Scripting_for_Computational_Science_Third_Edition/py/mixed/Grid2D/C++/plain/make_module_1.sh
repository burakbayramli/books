#!/bin/sh -x
rm -f *.o *.so
root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`
g++ -O3 -g -I. -I$numpy -I$root/include/python$ver -c gridloop.cpp
g++ -shared -o ext_gridloop.so gridloop.o

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop)'
           
