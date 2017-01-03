#!/bin/sh -x

# compile and link the demo program scxx_demo.cpp
#
# WARNING: The executable app.tmp may lead to a seg.fault.
#

rm -f *.o *.so
root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`
inc=$PYTHONSRC/../tools/scxx_b3
lib=$root/lib
g++ -O3 -g -I../plain -I$numpy -I$root/include/python$ver -I$inc \
    -c scxx_demo.cpp
g++ -o app.tmp -L$root/lib/python$ver/config -L$root/lib -L/usr/X11R6/lib \
    scxx_demo.o $root/lib/frobit.so \
    -lpython$ver -lz -lX11 -lpthread -ldl -lutil 

           
