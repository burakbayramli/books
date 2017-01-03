#!/bin/sh
# doesn't work:
#f2py -m tmp_nc -c --build-dir tmp1 somefile.f 

# first make signature file tmp.pyf:
f2py -m tmp -h tmp.pyf --overwrite-signature somefile.f
# edit signature file:
subst.py 'use r2__user__routines' 'use r1__user__routines, f2=>f1' tmp.pyf
# build extension module:
f2py -c tmp.pyf --build-dir tmp1 somefile.f 

# test:
python -c '
import tmp
from Numeric import *
def myfunc(x, y):
    y += x  # note: in-place modifications required!
p = zeros(2,Float) + 2.0;  q = p + 4
tmp.r1(p,q, myfunc)
print "result p, q :", p, q
print "should be   : [ 2.  2.] [ 8.  8.]"'

