# 3D adaptive quadrature over 1st octant of a torus
from math import *
from integral import *

#------------------------------------------------------------------ integrand
def Func(x, y, z):
   global R, r
   Rp = sqrt(x*x + y*y)                               # major radial distance
   dR = R - Rp
   rp = sqrt(dR*dR + z*z)                             # minor radial distance
   dr = 1e0 - rp/r
   return (dr*dr if rp <= r else 0e0)                          # zero-padding

#--------------------------------------------------------- integration limits
def ay(x):
   return (sqrt((R-r)*(R-r) - x*x) if x <= R-r else 0.0)
def by(x):
   return sqrt((R+r)*(R+r) - x*x)
def az(x, y):
   return 0.0
def bz(x, y):
   r0 = R - sqrt(x*x + y*y)
   return sqrt(fabs(r*r - r0*r0))

#----------------------------------------------------------------- interfaces
def Fz(z):                                                      # z-integrand
   global xvar, yvar
   return Func(xvar,yvar,z)                     # (x,y) as global (xvar,yvar)
def Fy(y):                                                      # y-integrand
   global xvar, yvar
   yvar = y                                                # stores y in yvar
   return qRomberg(Fz,az(xvar,y),bz(xvar,y),eps)           # x as global xvar
def Fx(x):                                                      # x-integrand
   global xvar
   xvar = x                                                # stores x in xvar
   return qRomberg(Fy,ay(x),by(x),eps)

# main

R = 3e0; r = 1e0
ax = 0e0; bx = R + r; eps = 1e-6
I = 8e0 * qRomberg(Fx,ax,bx,eps)                             # outer integral
print("Integral = ",I)                                   # result 9.869605...
