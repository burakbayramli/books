# Real roots of a real function by the bisection method
from math import *
from roots import *

def func(x):
   return (x*x*x*x - 5e0*x*x + 4e0) * sin(2e0*x)

# main

xmin = -3.5e0                                  # limits of root search domain
xmax =  3.5e0
h = 0.1e0                                # width of root separation intervals

a = xmin
while (a < xmax):                                      # root separation loop
   b = a + h                                          # search interval [a,b]
   (x,ierr) = Bisect(func,a,b)
   if ((ierr == 0) and (x != b)):
      print('x = {0:8.5f} in ({1:6.2f},{2:6.2f})  f(x) = {3:7.0e}'.
            format(x,a,b,func(x)))
   a = b                                                # shift left boundary
