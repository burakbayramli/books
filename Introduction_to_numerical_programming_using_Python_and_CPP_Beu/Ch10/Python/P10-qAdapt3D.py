# 3D adaptive quadrature over 1st octant of unity sphere
from math import *
from integral import *

#------------------------------------------------------------------ integrand
def Func(x, y, z): return 1e0

#--------------------------------------------------------- integration limits
def ay(x): return 0e0
def by(x): return sqrt(1e0 - x*x)
def az(x, y): return 0.0
def bz(x, y): return sqrt(fabs(1e0 - x*x - y*y))

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

ax = 0e0; bx = 1e0; eps = 1e-7

I = qRomberg(Fx,ax,bx,eps)                                   # outer integral
print("Integral = ",I)                                  # result 0.5235987...
