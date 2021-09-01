# Evaluates a 3D integral using spherical coordinates
from math import *
from integral import *

def Func(r, theta, phi):                   # probability density for 2p state
   c = 1e0/(32e0*pi)
   rcos = r * cos(theta)
   return c * rcos * rcos * exp(-r)

# main

a = 35e0                                                       # radial limit
nr = 150; nt = 60; np = 3
I = qSimpsonSph(Func,a,nr,nt,np)
print("I SimpsonSph = ",I) 

nr = 3; nt = 9; np = 1
I = qGaussSph(Func,nr,nt,np)
print("I GaussSph   = ",I)
