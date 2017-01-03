#!/bin/sh -x
root=`python -c 'import sys; print sys.prefix'`
ver=`python -c 'import sys; print sys.version[:3]'`
g++ -O3 -g -I. -I../plain -I$root/include/python$ver \
    -c test.cpp ../plain/NumPyArray.cpp
g++ -o app.tmp -L$root/lib/python$ver/config -L$root/lib -L/usr/X11R6/lib \
    test.o NumPyArray.o \
    -lpython$ver -lTix8.4 -lBLT -ltk8.4 -ltcl8.4 -lz -lX11 -lpthread -ldl -lutil 
