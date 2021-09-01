#  Eigenstates of the 1D Schroedinger equation for the harmonic oscillator
#     y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0, y(+inf) = 0
#     V(x) = 0.5*x*x
#  using a shooting algorithm based on the Numerov method
#----------------------------------------------------------------------------
from math import *
from ode import *
from specfunc import *

def Pot(x): return 0.5e0*x*x              # Potential for harmonic oscillator

# main

xx = 10e0                                                   # limit of x-mesh
xc = 6e0                        # checkpoint for vanishing solution (xc < xx)
par = 0                                          # parity of eigenstate (0/1)
Emin = 0e0                                      # lower limit for eigenvalues
Emax = 1e2                                      # upper limit for eigenvalues
dE = 0.1e0                                # minimum separation of eigenvalues
eps = 1e-4                                     # tolerance for solution at xc
hx = 1e-4                                                  # x-mesh step size

nx = int(xx/hx + 0.5) + 1                           # number of x-mesh points
nc = int(xc/hx + 0.5) + 1               # index of checkpoint for vanishing y
x = [0]*(nx+1); y = [0]*(nx+1)                             # x-mesh, solution
V = [0]*(nx+1)                                          # tabulated potential

for m in range(1,nx+1):
   x[m] = (m-1)*hx                                         # integration mesh
   V[m] = Pot(x[m])                                      # tabulate potential

Ew = Emin                    # lower limit of search window for E, [Ew,Ew+dE]
while (Ew < Emax):                             # loop over eigenvalue windows
                                                    # initial values at x = 0
   if (par == 0): y0 = 1e0; dy0 = 0e0                                # even y
   else:          y0 = 0e0; dy0 = 1e0                                 # odd y

   (E, exist) = ShootQM(Ew,Ew+dE,V,x,y,nx,nc,y0,dy0,eps)

   if (exist): break

   Ew += dE                                 # shift [Ew,Ew+dE] for next shoot

if (exist):
   n = int(E)                                                # quantum number

   f = 0e0                                  # normalize y by trapezoidal rule
   for m in range(1,nc+1): f += y[m]*y[m]*hx              # norm for [0,+inf]
   f = sqrt(2e0*f)
   if (int(n/2) % 2): f = -f                                # sign correction
   for m in range(1,nx+1): y[m] /= f

   f = sqrt(pi)                                 # norm of Hermite polynomials
   for i in range(1,n+1): f *= 2e0 * i
   f = 1e0/sqrt(f)

   out = open("shoot.txt","w")
   out.write("E{0:1d} = {1:8.5f}\n".format(n,E))
   out.write("      x        y{0:1d}        err\n".format(n))
   for m in range(1,nc+1):
      (yH, d) = Hermite(n,x[m]); yH *= f * exp(-0.5*x[m]*x[m])
      out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(x[m],y[m],yH-y[m]))
   out.close()
else:
   print("No solution found !")
