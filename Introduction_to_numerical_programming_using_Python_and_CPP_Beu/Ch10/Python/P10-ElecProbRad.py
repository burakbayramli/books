# Radial localization probability of an electron within the H atom
from math import *
from integral import *

def Func(r):                               # probability density for 2s state
   f = r * (2-r)
   return f * f * exp(-r) / 8e0

# main

eps = 1e-8                                  # relative precision of integrals

print("Radial localization probability:")
for R in range(10,31):
   I = qRomberg(Func,0e0,R,eps)
   print("[0,{0:.1f}] = {1:10.8f}".format(R,I)) 

print("\nTotal localization probability:")
Rinf = 25e0                                             # guess for +infinity
(I,Rinf) = qImprop1(Func,0e0,Rinf,eps)
print("I Improp1  = {0:10.8f}  Rinf = {1:.1f}".format(I,Rinf))

n = 3                                    # number of radial integration nodes
I = qGaussLag(Func,0e0,n)
print("I GaussLag = {0:10.8f}".format(I))
