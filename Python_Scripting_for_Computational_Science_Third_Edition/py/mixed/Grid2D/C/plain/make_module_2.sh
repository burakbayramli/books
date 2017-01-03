#!/bin/sh -x
./clean.sh

python setup.py build build_ext --inplace

# test the module:
python -c 'import ext_gridloop; print dir(ext_gridloop); print ext_gridloop.__doc__'
           