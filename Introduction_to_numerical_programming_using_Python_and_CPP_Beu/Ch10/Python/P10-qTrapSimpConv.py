# Convergence of trapezoidal and Simpson quadratures
from math import *
from integral import *

def Func(x): return sin(x)                                        # integrand

a = 0e0; b = pi                                          # integration domain

I = cos(a) - cos(b)                                            # exact result
print("Exact result = ",I)
print("   n       IT        errT   facT      IS        errS   facS")

n = 1
errT0 = 1e0; errS0 = 1e0
for i in range(1,11):
   n = 2*n                                              # number of intervals

   IT = qTrapz(Func,a,b,n+1)                               # trapezoidal rule
   errT = fabs(1e0 - IT/I)                                   # relative error
   facT = errT0/errT                                        # error reduction

   IS = qSimpson(Func,a,b,n+1)                               # Simpson's rule
   errS = fabs(1e0 - IS/I)                                   # relative error
   facS = errS0/errS                                        # error reduction

   print(("{0:5d}{1:11.6f}{2:10.1e}{3:6.1f}{4:11.6f}{5:10.1e}{6:6.1f}").
         format(n,IT,errT,facT,IS,errS,facS))

   errT0 = errT; errS0 = errS
