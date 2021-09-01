# Compare solvers for algebraic and transcendental equations
from math import *
from roots import *

def func(x): return (x - sin(x) - 0.25e0)

# main

a = -30e0; b = 30e0                                         # search interval
(x,ierr,it) = Bisect(func,a,b)              # add it to return list in Bisect
if (ierr == 0):
   print('Bisect: x = {0:8.5f}   f(x) = {1:7.0e}   it = {2:d}'.
         format(x,func(x),it))

x = -30e0                                             # initial approximation
(x,ierr,it) = Secant(func,a,b,x)            # add it to return list in Secant
if (ierr == 0):
   print('Secant: x = {0:8.5f}   f(x) = {1:7.0e}   it = {2:d}'.
         format(x,func(x),it))
