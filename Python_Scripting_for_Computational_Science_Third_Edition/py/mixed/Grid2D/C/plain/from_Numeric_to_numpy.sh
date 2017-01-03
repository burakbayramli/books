#!/bin/sh
#
# Here are some basic updates from Numeric C API to numpy C API,
# especially for in gridloop.c.

# not used in gridloop.c:
scitools subst PyArray_FromDimsAndData PyArray_SimpleNewFromData *.c

# these are needed for the Numeric version of gridloop.c:
scitools subst PyArray_DOUBLE NPY_DOUBLE *.c
scitools subst IND DIND *.c
scitools subst PyArray_FromDims PyArray_SimpleNew *.c
scitools subst Numeric numpy *.c

# other typical manual adjustments (not necessary in gridloop.c): 
# cast data to void* in PyArray_SimpleNewFromData
# rewrite PyArray_ContiguousFromObject (-> PyArray_FROM_OTF)

# compile and link:
# numpy=`python -c 'import numpy; print numpy.get_include()'`
# gcc ... -I$numpy
# setup.py: include_dirs=[..., numpy.get_include()]

# these are not required, but macros are recommended so we can do
# some very gridloop.c-specific substitutions:
scitools subst 'a->nd' 'PyArray_NDIM(a)' *.c
scitools subst 'xcoor->nd' 'PyArray_NDIM(xcoor)' *.c
scitools subst 'ycoor->nd' 'PyArray_NDIM(ycoor)' *.c
scitools subst 'a->dimensions\[0\]' 'PyArray_DIM(a,0)' *.c
scitools subst 'a->dimensions\[1\]' 'PyArray_DIM(a,1)' *.c
scitools subst 'xcoor->dimensions\[0\]' 'PyArray_DIM(xcoor,0)' *.c
scitools subst 'ycoor->dimensions\[0\]' 'PyArray_DIM(ycoor,0)' *.c
scitools subst 'a->descr->type_num' 'PyArray_TYPE(a)' *.c
scitools subst 'xcoor->descr->type_num' 'PyArray_TYPE(xcoor)' *.c
scitools subst 'ycoor->descr->type_num' 'PyArray_TYPE(ycoor)' *.c
scitools subst '\(a->data \+ i\*a->strides\[0\] \+ j\*a->strides\[1\]\)' ' PyArray_GETPTR2(a, i, j)' *.c
scitools subst '\(xcoor->data \+ i\*xcoor->strides\[0\]\)' ' PyArray_GETPTR1(xcoor, i)' *.c
scitools subst '\(ycoor->data \+ j\*ycoor->strides\[0\]\)' ' PyArray_GETPTR1(ycoor, j)' *.c

