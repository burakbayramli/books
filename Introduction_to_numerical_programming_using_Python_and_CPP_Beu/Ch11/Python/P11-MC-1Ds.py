# One-dimensional Monte Carlo quadrature with variance reduction
from math import *
from random import *

def ranSqrt():
#----------------------------------------------------------------------------
#  Returns a random number x in the range [0,1) with the distribution 
#  w(x) = 3/2 x^(1/2), and the corresponding value w(x)
#----------------------------------------------------------------------------
   x = pow(random(),2e0/3e0)
   w = 1.5e0 * sqrt(x)
   return (x, w)

def func(x): return x * exp(-x)                                   # integrand

# main

n = eval(input("n = "))                           # number of sampling points

seed()

f1 = f2 = 0e0                           # quadrature with importance sampling
for i in range(1,n+1):
   (x, w) = ranSqrt()                            # RNG with distribution w(x)
   if (w):
      f = func(x) / w                                             # integrand
      f1 += f; f2 += f * f                                             # sums

f1 /= n; f2 /= n                                                   # averages
s = f1                                                             # integral
sigma = sqrt((f2-f1*f1)/n)                               # standard deviation
print("s = ",s," +/- ",sigma)
