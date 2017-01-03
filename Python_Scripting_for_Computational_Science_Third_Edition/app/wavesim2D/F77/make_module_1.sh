#!/bin/sh
echo "This script is not updated; it employs an old interface file..."
exit 1

rm -rf tmp1
# make a Python loadable module out of the main.f program
# here we wrap all functions

# first transform F77WAVE.fcp, written with the aid of the
# C preprocessor, to plain F77:
./fcpp.py F77WAVE.fcp

#f2py -m wave2Dtmp -h wave2D.pyf main.f
# wave2D needs to be edited because the bottom and surface
# external functions are not used in calls in timeloop.
# wave2D.pyf is already edited correctly and is used
# in the command below to build the module.

# make_module_2.sh presents an alternative approach where
# only two functions are interfaced, and then f2py works
# without any interface file adjustments.

f2py -c wave2D.pyf --fcompiler='Gnu' --build-dir tmp1 \
   -DF2PY_REPORT_ON_ARRAY_COPY=1 main.f F77WAVE.f

python -c 'import wave2D; print wave2D.__doc__'
