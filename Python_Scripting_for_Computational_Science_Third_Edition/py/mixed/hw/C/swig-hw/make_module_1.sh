#!/bin/sh -x
./clean.sh
# compile C code and make a shared library module for use with Python
# method: manual compilation and linking

swig -python -I.. hw.i

root=`python -c 'import sys; print sys.prefix'`
ver=`python -c 'import sys; print sys.version[:3]'`
gcc -O -I.. -I$root/include/python$ver -c ../hw.c hw_wrap.c
gcc -shared -o _hw.so hw.o hw_wrap.o

python -c "import hw" # test
