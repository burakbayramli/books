#  Eigenstates of the 1D Schroedinger equation for the harmonic oscillator
#     y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0, y(+inf) = 0
#     V(x) = 0.5*x*x
#  using a shooting algorithm based on the Numerov method
#----------------------------------------------------------------------------
from math import *
from ode import *
from graphlib import *

def Pot(x): return 0.5e0*x*x              # Potential for harmonic oscillator

# main

xx = 10e0                                                   # limit of x-mesh
xc = 6e0                        # checkpoint for vanishing solution (xc < xx)
nE = 8                               # number of eigenvalues to be calculated
dE = 0.1e0                                # minimum separation of eigenvalues
eps = 1e-4                                     # tolerance for solution at xx
hx = 1e-3                                                  # x-mesh step size

nx = int(xx/hx + 0.5) + 1                           # number of x-mesh points
nc = int(xc/hx + 0.5) + 1               # index of checkpoint for vanishing y
x = [0]*(nx+1); y = [0]*(nx+1); y2 = [0]*(nx+1)            # x-mesh, solution
V = [0]*(nx+1)                                          # tabulated potential

Vmin = Vmax = Pot(xx)
for m in range(1,nx+1):
   x[m] = (m-1)*hx                                         # integration mesh
   V[m] = Pot(x[m])                                      # tabulate potential
   if (Vmin > V[m]): Vmin = V[m]                          # potential minimum
   if (Vmax < V[m]): Vmax = V[m]                          # potential maximum

GraphInit(800,800)

hy = 0.92e0/nE                               # fractional height of the plots
fy = 0.05                                # lower fractional position of plots

iE = 0                                                     # index of found E
par = 0                                              # parity of ground state
Ew = Vmin                    # lower limit of search window for E, [Ew,Ew+dE]
while (Ew < Vmax and iE < nE):                 # loop over eigenvalue windows
                                                    # initial values at x = 0
   if (par == 0): y0 = 1e0; dy0 = 0e0                                # even y
   else:          y0 = 0e0; dy0 = 1e0                                 # odd y
                                                        # shoot in [Ew,Ew+dE]
   (E, exist) = ShootQM(Ew,Ew+dE,V,x,y,nx,nc,y0,dy0,eps)

   Ew += dE                                 # shift [Ew,Ew+dE] for next shoot

   if (exist):
      iE += 1                                                   # found new E
      par = 0 if par else 1                                # parity of next y

      f = 0e0                               # normalize y by trapezoidal rule
      for m in range(1,nc+1): f += y[m]*y[m]*hx           # norm for [0,+inf]
      f = sqrt(2e0*f)
      if (int((iE-1)/2) % 2): f = -f                        # sign correction
      for m in range(1,nx+1): y[m] /= f; y2[m] = y[m]*y[m]

      title = "E{0:1d} = {1:4.2f}".format(iE-1,E)
      xtext = "x" if (iE == 1) else "None"
      Plot(x,y,nc,"blue",1,0.11,0.48,fy,fy+hy,xtext,"y",title)
      Plot(x,y2,nc,"blue",1,0.61,0.98,fy,fy+hy,xtext,"y^2",title)
      fy += hy                           # fractional y-position of next plot

MainLoop()
