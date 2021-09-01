# Norm of associated Legendre functions and spherical harmonics
from math import *
from specfunc import *
from integral import *

def aLegendre2(x):                    # squared normalized Legendre functions
   f = 1e0
   for i in range(l-m+1,l+m+1): f *= i                    # (l+|m|)!/(l-|m|)!
   f = sqrt((2*l+1)/(2e0*f)) * aLegendre(l,m,x)               # normalization
   return f * f

def SpherY2(theta, phi):                        # squared spherical harmonics
   (ReY,ImY) = SpherY(l,m,theta,phi)
   return ReY * ReY + ImY * ImY

# main

lmax = 5                                                    # maximum order l
nt = 181; np = 3       # number of mesh points for angular Simpson quadrature

for l in range(0,lmax+1):
  for m in range(0,l+1):
     NormL = qRomberg(aLegendre2,-1e0,1e0,1e-10)
     NormY = qSimpsonAng(SpherY2,nt,np)
     print("l = {0:d} m = {1:d} NormL = {2:13.10f} NormY = {3:13.10f}".
           format(l,m,NormL,NormY))
