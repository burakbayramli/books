#!/bin/sh -x
#./clean.sh

swig -python -c++ -I. matrix_cpp.i

root=`python -c 'import sys; print sys.prefix'`
ver=`python -c 'import sys; print sys.version[:3]'`
g++ -O -I. -I$root/include/python$ver -c matrix_cpp.cpp matrix_cpp_wrap.cxx
g++ -shared -o _matrix_cpp.so matrix_cpp.o matrix_cpp_wrap.o

python -c "import matrix_cpp" # test
