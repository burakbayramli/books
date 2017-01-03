#!/bin/sh
# make a Python loadable module out of the main.f program,
# here we wrap only the solveatthistimestep and dump functions

# first transform F77WAVE.fcp, written with the aid of the
# C preprocessor, to plain F77:
./fcpp.py F77WAVE.fcp

f2py -m wave2D -c --fcompiler='Gnu' --build-dir tmp1 \
   -DF2PY_REPORT_ON_ARRAY_COPY=1 main.f F77WAVE.f only: solveatthistimestep dump

python -c 'import wave2D; print wave2D.__doc__'