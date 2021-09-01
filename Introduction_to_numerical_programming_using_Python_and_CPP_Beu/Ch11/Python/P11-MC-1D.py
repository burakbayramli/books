# One-dimensional Monte Carlo quadrature
from math import *
from random import *

def func(x): return x * exp(-x)                                   # integrand

# main

n = eval(input("n = "))                           # number of sampling points

seed()

f1 = f2 = 0e0                              # quadrature with uniform sampling
for i in range(1,n+1):
   x = random()                               # RNG with uniform distribution
   f = func(x)                                                    # integrand
   f1 += f; f2 += f * f                                                # sums

f1 /= n; f2 /= n                                                   # averages
s = f1                                                             # integral
sigma = sqrt((f2-f1*f1)/n)                               # standard deviation
print("s = ",s," +/- ",sigma)
