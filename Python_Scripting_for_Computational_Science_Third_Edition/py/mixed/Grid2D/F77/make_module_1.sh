#!/bin/sh
f2py -m ext_gridloop -c --fcompiler=gfortran --build-dir tmp1 \
     -DF2PY_REPORT_ON_ARRAY_COPY=1 gridloop.f

python -c 'import ext_gridloop; print dir(ext_gridloop); print ext_gridloop.__doc__'
