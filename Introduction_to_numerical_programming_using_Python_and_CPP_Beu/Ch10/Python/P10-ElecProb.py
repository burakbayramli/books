# Total localization probability of an electron in the H atom
from math import *
from integral import *

def Func(r, theta, phi):                 # probability density for 3d_0 state
   c = 1e0/(81e0*sqrt(6e0*pi))
   cost = cos(theta)
   psi = c * r * r * exp(-r/3e0) * (3e0*cost*cost - 1e0)
   return psi * psi

# main

a = 40e0                                                       # radial limit
nr = 100; nt = 70; np = 3
I = qSimpsonSph(Func,a,nr,nt,np)
print("I SimpsonSph = ",I) 

nr = 10; nt = 10; np = 1
I = qGaussSph(Func,nr,nt,np)
print("I GaussSph   = ",I)
