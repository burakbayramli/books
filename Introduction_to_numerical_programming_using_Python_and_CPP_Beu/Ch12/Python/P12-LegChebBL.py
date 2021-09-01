# Legendre and Chebyshev polynomials by the finite-difference method
from math import *
from ode import *
from graphlib import *

def Func(x):                                            # RHS of Legendre ODE
   p = 2e0*x/(1e0-x*x); q =-n*(n+1)/(1e0-x*x); r = 0e0
   return (p, q, r)

def Func1(x):                                          # RHS of Chebyshev ODE
   p = x/(1e0-x*x); q =-n*n/(1e0-x*x); r = 0e0
   return (p, q, r)

# main

nmax = 5                                           # max. order of polynomial
xa = -1e0;  xb = 1e0                                      # domain boundaries
hx = 1e-3                                                  # x-mesh step size

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
   alf1 = -1e0 if n % 2 else 1e0; bet1 = 0e0           # Dirichlet conditions
   alf2 = 1e0; bet2 = 0e0
   Bilocal(xa,xb,y,nx,alf1,bet1,alf2,bet2,Func)

   nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1
   m0 = (n-1)*nx
   for m in range(1,nx+1): xp[m0+m] = x[m]; yp[m0+m] = y[m]

MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1, \
          0.10,0.45,0.15,0.85,"x","Pn", \
          "Legendre polynomials - finite-differences")
                                                      # Chebyshev polynomials
for n in range(1,nmax+1):                        # loop over polynomial order
   alf1 = -1e0 if n % 2 else 1e0; bet1 = 0e0           # Dirichlet conditions
   alf2 = 1e0; bet2 = 0e0
   Bilocal(xa,xb,y,nx,alf1,bet1,alf2,bet2,Func1)

   nn[n] = n*nx; col[n] = color[(n-1)%5]; sty[n] = 1
   m0 = (n-1)*nx
   for m in range(1,nx+1): xp[m0+m] = x[m]; yp[m0+m] = y[m]

MultiPlot(xp,yp,yp,nn,col,sty,nmax,10,0e0,0e0,0,-1e0,1e0,1, \
          0.60,0.95,0.15,0.85,"x","Tn", \
          "Chebyshev polynomials - finite-differences")

MainLoop()
