# Evaluates a 3D integral on a torus using Cartesian coordinates
from math import *
from integral import *

def Func(x, y, z):
   global R, r
   Rp = sqrt(x*x + y*y)                               # major radial distance
   dR = R - Rp
   rp = sqrt(dR*dR + z*z)                             # minor radial distance
   dr = 1e0 - rp/r
   return (dr*dr if rp <= r else 0e0)                          # zero-padding

# main

R = 3e0; r = 1e0                                  # major & minor torus radii
ax = ay = az = 0e0                                 # bounding box: 1st octant
bx = by = R+r; bz = r                               # multiply results with 8
nx = ny = 200                                         # equal density of mesh
nz = int(nx * r/(R+r))                                 # points along x, y, z

I = 8e0 * qSimpson3D(Func,ax,bx,nx,ay,by,ny,az,bz,nz)
print("I Simpson  = ",I)
I = 8e0 * qGaussLeg3D(Func,ax,bx,nx,ay,by,ny,az,bz,nz)
print("I GaussLeg = ",I)                                 # result 9.869603...
