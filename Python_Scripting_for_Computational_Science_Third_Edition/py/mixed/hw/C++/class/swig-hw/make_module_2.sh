#!/bin/sh -x
./clean.sh
# compile C code and make a shared library module for use with Python
# method: run setup.py

python setup.py build build_ext --inplace

python -c "import hw; print dir(hw); print dir(hw.HelloWorld)" # test


