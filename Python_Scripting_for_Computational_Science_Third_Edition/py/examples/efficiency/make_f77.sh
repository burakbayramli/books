#!/bin/sh
f2py -m matrix_f77 -DF2PY_REPORT_ON_ARRAY_COPY=1 -DF2PY_REPORT_ATEXIT \
         -c matrix_f77.f \
         only: makematrix set set_a get fill1 lfill1 fill2 lfill2 tonumpy adump 
f2py -m call -DF2PY_REPORT_ON_ARRAY_COPY=1 -DF2PY_REPORT_ATEXIT -c call.f

python -c 'import matrix_f77, call; print "\n\n\n", matrix_f77.__doc__, "\n\n", call.__doc__'

