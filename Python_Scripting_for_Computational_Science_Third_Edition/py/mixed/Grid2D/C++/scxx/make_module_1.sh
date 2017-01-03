#!/bin/sh -x
rm -f *.o *.so
root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`
inc=$PYTHONSRC/../tools/scxx_b3
lib=$root/lib
g++ -O3 -g -I../plain -I$numpy -I$root/include/python$ver -I$inc \
    -c gridloop_scxx.cpp
g++ -shared -o ext_gridloop.so gridloop_scxx.o $lib/frobit.so

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop)'
           
