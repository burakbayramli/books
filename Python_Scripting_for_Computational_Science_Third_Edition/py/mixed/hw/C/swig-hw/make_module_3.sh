#!/bin/sh -x
./clean.sh
# compile C code and make a shared library module for use with Python
# method: manual compilation and linking
# this script applies the interface file hw2.i instead hw.i
# (hw2.i is more compact)

swig -python -I.. hw2.i

root=`python -c 'import sys; print sys.prefix'`
ver=`python -c 'import sys; print sys.version[:3]'`
gcc -O -I.. -I$root/include/python$ver -c ../hw.c hw2_wrap.c
gcc -shared -o _hw.so hw.o hw2_wrap.o

python -c "import hw" # test

