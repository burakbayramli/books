# Real roots of a real function by the Newton-Raphson method
from math import *
from roots import *

def func(x):
   df = 1e0 + exp(-x)
   return (x - exp(-x), df)

def func1(x):
   df = 1e0 - cos(x)
   return (x - sin(x) - 0.25e0, df)

def func2(x):
   df = (4e0*x*x*x - 10e0*x) * sin(2e0*x) \
      + (x*x*x*x - 5e0*x*x + 4e0) * 2e0*cos(2e0*x)
   return ((x*x*x*x - 5e0*x*x + 4e0) * sin(2e0*x), df)

# main

xmin = -3.5e0                                  # limits of root search domain
xmax =  3.5e0
h = 0.1e0                                # width of root separation intervals

a = xmin
while (a < xmax):                                      # root separation loop
   b = a + h                                          # search interval [a,b]
   x = 0.5*(a+b)                                      # initial approximation
   (x,ierr) = Newton(func,a,b,x)
   if ((ierr == 0) and (x != b)):
      print('x = {0:8.5f} in ({1:6.2f},{2:6.2f})  f(x) = {3:7.0e}'.
            format(x,a,b,func(x)[0]))
   a = b                                                # shift left boundary
