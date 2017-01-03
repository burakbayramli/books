#!/usr/bin/python
## example2_7
from numarray import zeros,array,Float64,product,diagonal
from LUdecomp import *

a = array([[ 3.0, -1.0,  4.0], \
           [-2.0,  0.0,  5.0], \
           [ 7.0,  2.0, -2.0]])
b = array([[ 6.0,  3.0,  7.0], \
           [-4.0,  2.0, -5.0]])
a = LUdecomp(a)
det = product(diagonal(a))
print "\nDeterminant =",det
for i in range(len(b)):
   x = LUsolve(a,b[i])
   print "x",i+1,"=",x
raw_input("\nPress return to exit")

