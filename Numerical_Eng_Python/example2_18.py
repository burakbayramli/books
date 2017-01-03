#!/usr/bin/python
## example2_18
from numarray import zeros,Float64,sqrt
from conjGrad import *

def Ax(v):
    n = len(v)
    Ax = zeros((n),type=Float64)
    Ax[0] = 2.0*v[0] - v[1]+v[n-1]
    Ax[1:n-1] = -v[0:n-2] + 2.0*v[1:n-1] -v [2:n]
    Ax[n-1] = -v[n-2] + 2.0*v[n-1] + v[0]
    return Ax

n = eval(raw_input("Number of equations ==> "))
b = zeros((n),type=Float64)
b[n-1] = 1.0
x = zeros((n),type=Float64)
x,numIter = conjGrad(Ax,x,b)
print "\nThe solution is:\n",x
print "\nNumber of iterations =",numIter
raw_input("\nPress return to exit")
    
