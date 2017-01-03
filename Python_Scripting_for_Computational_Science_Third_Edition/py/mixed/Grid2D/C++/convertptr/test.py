#!/usr/bin/env python

import ext_gridloop
from numpy import linspace, zeros
c = ext_gridloop.Convert_MyArray()
n = 3
a = zeros((n,n))
xcoor = linspace(0, 1, 3)
ycoor = xcoor.copy
def f(x, y):
    print "in f(%g,%g)" % (x,y)
    return x+y+1

x_p = c.py2my(xcoor)
y_p = c.py2my(ycoor)
a_p = c.py2my(a)

x_p = c.py2my(xcoor)
y_p = c.py2my(ycoor)
a_p = c.py2my_copy(a)

f_p = c.set_pyfunc(f)
print "calling gridloop1"
ext_gridloop.gridloop1(a_p, x_p, y_p, f_p)

b = c.my2py_copy(a_p)
print "b=", b

print "calling gridloop2"
a2_p = ext_gridloop.gridloop2(x_p, y_p, f_p)
print "converting C++ ptr to NumPy"
a2 = c.my2py(a2_p)
print "a2", a2

