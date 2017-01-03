#!/usr/bin/env python
import os, sys

# run
# f2py -m ext_gridloop -c --fcompiler='Gnu' gridloop.f

from numpy import *

# local data, as in Grid2D:
xcoor = linspace(0, 1, 3)
ycoor = linspace(0, 1, 2)
a = zeros((xcoor.size, ycoor.size))
print 'a is', type(a)
print 'x =', xcoor, '\ny =', ycoor

def dump():
    for i in range(a.shape[0]):
        for j in range(a.shape[1]):
            print 'value at (%g,%g)  \t = a[%d,%d] = %g' % \
                  (xcoor[i], ycoor[j], i, j, a[i,j])
def myfunc(x, y):
    r = x + 2*y
    print 'in myfunc...return', r
    return r

import ext_gridloop

print ext_gridloop.gridloop1_v1.__doc__
ext_gridloop.gridloop1_v1(a, xcoor, ycoor, myfunc)
print 'a after gridloop_v1:\n', a
dump()
print 'a is still filled with zeros!\n\n'

# try something simple and compare 1-dim and 2-dim arrays:
print ext_gridloop.change.__doc__
ext_gridloop.change(a, xcoor, ycoor)
print 'a after change:\n', a
print 'xcoor after change:\n', xcoor
print 'ycoor after change:\n', ycoor

# repair x and y (a is ok in the calling code):
xcoor = linspace(0, 1, 3)
ycoor = linspace(0, 1, 2)


print ext_gridloop.gridloop2.__doc__
a = ext_gridloop.gridloop2(xcoor, ycoor, myfunc)
print 'a after gridloop2:\n', a
dump()
print 'a is correct!\n\n'

# work with an input _and_ output array:
print ext_gridloop.gridloop3.__doc__
a[:,:] = -1.0
print 'Fortran storage of a?', \
      ext_gridloop.has_column_major_storage(a)
b = ext_gridloop.gridloop3(a, xcoor, ycoor, myfunc)
print 'a after gridloop3:\n', a
print 'is a overwritten?', a is b
dump()
print 'a is correct!\n\n'

# work with an overwrite statement:
print ext_gridloop.gridloop4.__doc__
a = zeros((xcoor.size, ycoor.size))  # C storage
print 'NumPy array; Fortran storage of a?', isfortran(a)
a = asarray(a, order='Fortran')
a[:,:] = -1.0
b = ext_gridloop.gridloop4(a, xcoor, ycoor, myfunc)
print 'a after gridloop4:\n', a
print 'is a overwritten?', a is b
dump()
print 'a is correct!\n\n'
# let a be a NumPy array with C storage:
a = zeros((xcoor.size, ycoor.size))
print 'Fortran storage of a?', isfortran(a)
b = ext_gridloop.gridloop4(a, xcoor, ycoor, myfunc)
print 'b after gridloop4:\n', b
print 'is a overwritten?', a is b
print 'b is correct!\n\n'

# work with an inout array (imporant: start with Fortran
# column major storage)
a = zeros((xcoor.size, ycoor.size), order='Fortran')
print ext_gridloop.gridloop1_v2.__doc__
print 'Fortran storage of a?', isfortran(a)
ext_gridloop.gridloop1_v2(a, xcoor, ycoor, myfunc)
print 'a after gridloop1_v2:\n', a
dump()
print 'a is correct!\n\n'

# try again, this time with a fresh NumPy (C) array as input:
a = zeros((xcoor.size, ycoor.size))
try:
    print 'Fortran storage of a?', isfortran(a)
    ext_gridloop.gridloop1_v2(a, xcoor, ycoor, myfunc)
except:
    pass

a = zeros((xcoor.size, ycoor.size), order='Fortran')
ext_gridloop.gridloop1_v2(a, xcoor, ycoor, myfunc)
print 'a after gridloop1_v2 with a c2fdim2 transformation:\n', a
dump()
print 'a is correct!\n\n'
# this is the way to realize the gridloop1 function!!


# try gridloop_v3, which declares a as intent(inout,c):
a = zeros((xcoor.size, ycoor.size))
print ext_gridloop.gridloop1_v3.__doc__
ext_gridloop.gridloop1_v3(a, xcoor, ycoor, myfunc)
print 'a after gridloop1_v3:\n', a
print 'Fortran storage of a?', isfortran(a)
dump()
print 'a is NOT correct!\n\n'

# try gridloop_v4, which declares a as intent(inout,c),
# but we operate on the transpose of a:
a = zeros((xcoor.size, ycoor.size))
print ext_gridloop.gridloop1_v4.__doc__
print 'Fortran storage of a?', isfortran(a)
ext_gridloop.gridloop1_v4(a, xcoor, ycoor, myfunc)
print 'a after gridloop1_v4:\n', a
print 'Fortran storage of a?', isfortran(a)
dump()
print 'a is correct!\n\n'



