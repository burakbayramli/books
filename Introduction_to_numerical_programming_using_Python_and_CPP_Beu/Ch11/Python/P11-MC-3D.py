# Monte Carlo calculation of the mass center of a torus of radii R and r,
# centered at the origin and with Oz as symmetry axis
from math import *
from random import *

def Func(x, y, z):
   global R, r
   Rp = sqrt(x*x + y*y)                               # major radial distance
   dR = R - Rp
   rp = sqrt(dR*dR + z*z)                             # minor radial distance
   dr = 1e0 - rp/r
   return (dr*dr if rp <= r else 0e0)                          # zero-padding

# main

R = 3e0; r = 1e0                                  # major & minor torus radii
Lx = Ly = R + r; Lz = r                         # extended domain: 1st octant
V = 8e0 * Lx * Ly * Lz                      # volume of total extended domain

n = 10000000                                      # number of sampling points

seed()

sm  = sx  = sy  = sz  = 0e0
sm2 = sx2 = sy2 = sz2 = 0e0
for i in range(1,n+1):
   x = Lx * (2e0*random() - 1e0)                             # -Lx <= x <= Lx
   y = Ly * (2e0*random() - 1e0)                             # -Ly <= y <= Ly
   z = Lz * (2e0*random() - 1e0)                             # -Lz <= x <= Lz
   dens = Func(x,y,z)                                               # density
   if (dens):
      f = dens    ; sm += f; sm2 += f * f                              # sums
      f = dens * x; sx += f; sx2 += f * f
      f = dens * y; sy += f; sy2 += f * f
      f = dens * z; sz += f; sz2 += f * f

sm /= n; sx /= n; sy /= n; sz /= n                                 # averages
m  = V * sm; sigm = V * sqrt((sm2/n - sm*sm)/n); f = V/m          # integrals
xc = f * sx; sigx = f * sqrt((sx2/n - sx*sx)/n)
yc = f * sy; sigy = f * sqrt((sy2/n - sy*sy)/n)
zc = f * sz; sigz = f * sqrt((sz2/n - sz*sz)/n)

print("m  = {0:8.5f} +/- {1:8.5f}".format(m ,sigm))
print("xc = {0:8.5f} +/- {1:8.5f}".format(xc,sigx))
print("yc = {0:8.5f} +/- {1:8.5f}".format(yc,sigy))
print("zc = {0:8.5f} +/- {1:8.5f}".format(zc,sigz))
