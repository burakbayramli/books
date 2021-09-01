# Evaluates an integral using the trapezoidal rule
from math import *

def Func(x): return (x*x*x) * exp(-x)

#============================================================================
def qTrapz(Func, a, b, n):
#----------------------------------------------------------------------------
#  Integrates function Func on interval [a,b] using the trapezoidal rule
#  with n integration points
#----------------------------------------------------------------------------
   h = (b-a)/(n-1)
   s = 0.5*(Func(a) + Func(b))
   for i in range(1,n-1): s += Func(a+i*h)

   return h*s

# main

a = 0e0; b = 1e0; n = 100
print("I = ",qTrapz(Func,a,b,n))
