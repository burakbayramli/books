# Convergence of trapezoidal and Simpson quadratures
from math import *
from integral import *
from specfunc import *

def Func(x):                         # integrand: norm of Legendre polynomial
    (f,d) = Legendre(nord,x)
    return f * f

nord = 4                                       # order of Legendre polynomial
a = -1e0; b = 1e0                                        # integration domain

I = 2e0/(2e0*nord+1e0)                                         # exact result
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
