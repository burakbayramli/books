# Legendre and Chebyshev polynomials by the shooting method
from math import *
from ode import *
from graphlib import *

def Func(x, y, dy):                                     # RHS of Legendre ODE
   return (2e0*x*dy - n*(n+1)*y) / (1e0 - x*x)

def Func1(x, y, dy):                                   # RHS of Chebyshev ODE
   return (x*dy - n*n*y) / (1e0 - x*x)

# main

nmax = 5                                           # max. order of polynomial
xa = 0e0                                                    # boundary values
xb = 1e0; yb = 1e0
eps = 1e-6                                     # tolerance for solution at xb
hx = 1e-4                                                  # x-mesh step size

nx = int((xb-xa)/hx + 0.5) + 1                      # number of x-mesh points

x = [0]*(nx+1); y = [0]*(nx+1)                             # x-mesh, solution
xp = [0]*(nx*nmax+1); yp = [0]*(nx*nmax+1)                  # plotting arrays
nn  = [0]*(nmax+1)                                  # ending indexes of plots
sty = [0]*(nmax+1)                                          # styles of plots
col = [""]*(nmax+1)                                         # colors of plots
color = ["blue", "cyan", "green", "orange", "red"]

for m in range(1,nx+1): x[m] = xa + (m-1)*hx                # generate x-mesh

GraphInit(1200,600)
                                                       # Legendre polynomials
for n in range(1,nmax+1):                        # loop over polynomial order
   if (n % 2 == 0):                               # even solutions: rescaling
      ya = 1e0; dy = 0e0
      Propag(x,y,nx,ya,dy,Func)
      for m in range(1,nx+1): y[m] /= y[nx]                   # normalization
   else:                                            # odd solutions: shooting
      ya = 0e0
      dy1 = -1e3; dy2 = 1e3         # search initial derivative in [dy1,dy2]
      (dy, exist) = Shoot(x,y,nx,ya,yb,dy1,dy2,eps,Func)

   nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1
   m0 = (n-1)*nx
   for m in range(1,nx+1): xp[m0+m] = x[m]; yp[m0+m] = y[m]

MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1, \
          0.10,0.45,0.15,0.85,"x","Pn", \
          "Legendre polynomials - shooting method")
                                                      # Chebyshev polynomials
for n in range(1,nmax+1):                        # loop over polynomial order
   if (n % 2 == 0):                               # even solutions: rescaling
      ya = 1e0; dy = 0e0
      Propag(x,y,nx,ya,dy,Func1)
      for m in range(1,nx+1): y[m] /= y[nx]                   # normalization
   else:                                            # odd solutions: shooting
      ya = 0e0
      dy1 = -1e3; dy2 = 1e3          # search initial derivative in [dy1,dy2]
      (dy, exist) = Shoot(x,y,nx,ya,yb,dy1,dy2,eps,Func1)

   nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1
   m0 = (n-1)*nx
   for m in range(1,nx+1): xp[m0+m] = x[m]; yp[m0+m] = y[m]

MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1, \
          0.60,0.95,0.15,0.85,"x","Tn", \
          "Chebyshev polynomials - shooting method")

MainLoop()
