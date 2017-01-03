#!/bin/sh -x
./clean.sh
# compile C code and make a shared library module for use with Python
# method: manual compilation and linking

swig -python -c++ -I.. hw.i

root=`python -c 'import sys; print sys.prefix'`
ver=`python -c 'import sys; print sys.version[:3]'`
g++ -O -I.. -I$root/include/python$ver \
    -c ../HelloWorld.cpp ../HelloWorld2.cpp hw_wrap.cxx
g++ -shared -o _hw.so HelloWorld.o HelloWorld2.o hw_wrap.o

python -c "import hw; print dir(hw); print dir(hw.HelloWorld); print dir(hw.HelloWorld2)" # test
