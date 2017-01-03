#!/bin/sh -x
#
# run an erroneous version of gridloop.c
# first command-line argument: number 1-5, corresponding to gridloop1.c,
# gridloop2.c, etc.
# second command-line argument: options to the gcc compiler (can be empty)

./clean.sh
if [ $# -lt 1 ]; then
    echo "make_module_1.sh X"
    echo "(X is a number 1-5)"
    exit
fi
modulefile=$1
options=$2

root=`python -c 'import sys; print sys.prefix'`
numpy=`python -c 'import numpy; print numpy.get_include()'`
ver=`python -c 'import sys; print sys.version[:3]'`

gcc -O3 -g -I$numpy -I$root/include/python$ver \
    -I$scripting/src/C $options \
    -c gridloop$modulefile.c -o gridloop.o

gcc -shared -o ext_gridloop.so gridloop.o 

python -c 'import ext_gridloop; print dir(ext_gridloop); \
           print ext_gridloop.__doc__'

# run the calling script in a debugger:
gdb `which python` <<EOF
run ../../../Grid2Deff.py verify1
where
^D
EOF
