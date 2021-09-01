# Legendre polynomials by the finite-difference method
from math import *
from ode import *
from specfunc import *

def Func(x):                                            # RHS of Legendre ODE
   global n                                                # polynomial order
   p = 2e0*x/(1e0-x*x); q =-n*(n+1)/(1e0-x*x); r = 0e0
   return (p, q, r)

# main

n = 5                                          # order of Legendre polynomial
xa = -1e0;  xb = 1e0                                      # domain boundaries
hx = 1e-3                                                  # x-mesh step size

nx = int((xb-xa)/hx + 0.5) + 1                      # number of x-mesh points

x = [0]*(nx+1); y = [0]*(nx+1)                             # x-mesh, solution

for m in range(1,nx+1): x[m] = xa + (m-1)*hx                # generate x-mesh

alf1 = -1e0 if n % 2 else 1e0; bet1 = 0e0              # Dirichlet conditions
alf2 = 1e0; bet2 = 0e0

Bilocal(xa,xb,y,nx,alf1,bet1,alf2,bet2,Func)

out = open("bilocal.txt","w")
out.write("      x        P{0:1d}        err\n".format(n))
for m in range(1,nx+1):
   (P, d) = Legendre(n,x[m])
   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(x[m],y[m],P-y[m]))
out.close()
