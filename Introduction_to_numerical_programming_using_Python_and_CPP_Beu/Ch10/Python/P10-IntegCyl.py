# Evaluates a 3D integral using cylindrical coordinates
from math import *
from integral import *

def Func(r, phi, z):
   return exp(-r) * pow(cos(phi),2) * z

# main

a = 35e0                                                       # radial limit
az = 0e0; bz = 1e0                                             # axial limits
nr = 200; np = 20; nz = 2                            # numbers of mesh points

I = qSimpsonCyl(Func,a,az,bz,nr,np,nz)
print("I SimpsonCyl = ",I)
print("I exact      = ",pi/2)
