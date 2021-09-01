# Real root of a real function by the method of successive approximations
from math import *
from roots import *

def func(x): return x - exp(-x)

# main

a = -1e10; b = 1e10                                         # search interval
x = 0e0                                               # initial approximation

(x,ierr) = Iter(func,a,b,x)
if (ierr == 0):
   print("x = {0:8.5f}   f(x) = {1:7.0e}".format(x,func(x)))
else: print("No solution found !")
