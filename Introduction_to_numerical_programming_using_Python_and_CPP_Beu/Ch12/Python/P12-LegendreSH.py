# Legendre polynomials by the shooting method
from math import *
from ode import *
from specfunc import *

def Func(x, y, dy):                                     # RHS of Legendre ODE
   global n                                                # polynomial order
   return (2e0*x*dy - n*(n+1)*y) / (1e0 - x*x)

# main

n = 5                                          # order of Legendre polynomial
xa = 0e0                                                    # boundary values
xb = 1e0; yb = 1e0
eps = 1e-4                                     # tolerance for solution at xb
hx = 1e-4                                                  # x-mesh step size

nx = int((xb-xa)/hx + 0.5) + 1                      # number of x-mesh points

x = [0]*(nx+1); y = [0]*(nx+1)                             # x-mesh, solution

for m in range(1,nx+1): x[m] = xa + (m-1)*hx                # generate x-mesh

if (n % 2 == 0):                                  # even solutions: rescaling
   ya = 1e0; dy = 0e0
   Propag(x,y,nx,ya,dy,Func)
   for m in range(1,nx+1): y[m] /= y[nx]                      # normalization
else:                                               # odd solutions: shooting
   ya = 0e0
   dy1 = -1e3; dy2 = 1e3             # search initial derivative in [dy1,dy2]
   (dy, exist) = Shoot(x,y,nx,ya,yb,dy1,dy2,eps,Func)
   
out = open("shoot.txt","w")
out.write("dy = {0:8.5f}\n".format(dy))
out.write("      x        P{0:1d}        err\n".format(n))
for m in range(1,nx+1):
   (P, d) = Legendre(n,x[m])
   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(x[m],y[m],P-y[m]))
out.close()
