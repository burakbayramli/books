#!/bin/sh -x
f2py -m ext_gridloop -h ext_gridloop.pyf --overwrite-signature signatures.f

f2py -c --fcompiler=gfortran --build-dir tmp1 \
     -DF2PY_REPORT_ON_ARRAY_COPY=1 ext_gridloop.pyf gridloop.c

python -c 'import ext_gridloop; print dir(ext_gridloop); print ext_gridloop.__doc__'
