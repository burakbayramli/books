# Gaussian quadratures
from math import *
from integral import *

def Func1(x): return 12*pow(x,11)
def Func2(x): return x*x*x * exp(-x)

# main

nmax = 6
x = [0]*(nmax+1)
w = [0]*(nmax+1)

print("Integral of 12*x^11 on [-1,1]")
for n in range(2,nmax+1):
   print("n = ",n,"I1 = ",qGaussLeg(Func1,0e0,1e0,n))

print("\nIntegral of x^3 * exp(-x) on [0,+inf)")
for n in range(2,nmax+1):
   print("n = ",n,"I2 = ",qGaussLag(Func2,0e0,n))
