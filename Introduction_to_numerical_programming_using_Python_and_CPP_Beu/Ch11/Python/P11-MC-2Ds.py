# Two-dimensional Monte Carlo quadrature with variance reduction
from math import *
from random import *
from random1 import *

def func(x,y):                                                    # integrand
   return (x*x + y*y)*exp(-0.5e0*(x*x + y*y))/(4e0*pi)

# main

L = 8e0                                  # integration domain [-L,L] x [-L,L]
L2 = L * L                                          # area of sampling domain

n = 100000                                        # number of sampling points

seed()

f1 = f2 = 0e0                              # quadrature with uniform sampling
for i in range(1,n+1):
   x = L * random(); y = L * random()
   f = func(x,y)                                                  # integrand
   f1 += f; f2 += f * f                                                # sums

f1 /= n; f2 /= n                                                   # averages
s = 4e0 * L2 * f1                                                  # integral
sigma = 4e0 * L2 * sqrt((f2-f1*f1)/n)                    # standard deviation
print("Uniform sampling : s = ",s," +/- ",sigma)

f1 = f2 = 0e0                           # quadrature with importance sampling
for i in range(1,n+1):
   (w, x, y) = randNrm2()           # random numbers with normal distribution
   f = func(x,y) / w                                              # integrand
   f1 += f; f2 += f * f                                                # sums

f1 /= n; f2 /= n                                                  # averages
s = f1                                                            # integral
sigma = sqrt((f2-f1*f1)/n)                              # standard deviation
print("Gaussian sampling: s = ",s," +/- ",sigma)
