# Solve 1D wave equation by the explicit finite-difference method
from math import *
from pde import *

#============================================================================
def Init(u0, u1, x, nx, c, dk, hx, ht):
#----------------------------------------------------------------------------
#  Returns initial solutions u0 and u1, for the first two time steps
#     u0(x,0) = sin(x*dk) / (x*dk)                           initial solution
#     v0(x,0) = 0                                     initial time derivative
#  x - spatial mesh, nx - number of nodes
#  c - phase velocity of wave, dk - wave number interval
#  hx - x-spacing, ht - time step size
#----------------------------------------------------------------------------
   for i in range(1,nx+1):                                      # time step 0
      u0[i] = sin(dk*x[i])/(dk*x[i]) if dk*x[i] else 1e0   # initial solution

   lam = c*ht/hx; lam = lam*lam                                 # time step 1
   lam2 = 2e0*(1e0 - lam)
   u1[1] = u0[1]; u1[nx] = u0[nx]                  # constant boundary values
   for i in range(2,nx):
      v0 = 0e0                                      # initial time derivative
      u1[i] = 0.5e0*(lam*u0[i-1] + lam2*u0[i] + lam*u0[i+1]) - ht*v0

# main

c    = 10e0                                             # phase speed of wave
dk   = 1e0                                             # wave number interval
xmax = 100e0                                                      # maximum x
hx   = 5e-2                                               # spatial step size
tmax = 40e0                                        # maximum propagation time
ht   = 5e-3                                                       # time step
nout = 500                                          # output every nout steps

nx = 2*(int)(xmax/hx + 0.5) + 1                 # odd number of spatial nodes
nt = (int)(tmax/ht + 0.5)                              # number of time steps
nx2 = int(nx/2)

u0 = [0]*(nx+1)                                            # initial solution
u1 = [0]*(nx+1)                                   # first propagated solution
u  = [0]*(nx+1)                                                    # solution
x  = [0]*(nx+1)                                                # spatial mesh

for i in range(1,nx+1): x[i] = (i-nx2-1)*hx                    # spatial mesh

Init(u0,u1,x,nx,c,dk,hx,ht)                             # initial wave packet

for it in range(1,nt+1):                                          # time loop
   t = it*ht
   PropagWave(u0,u1,u,nx,c,hx,ht)                        # propagate solution

   for i in range(1,nx): u0[i] = u1[i];  u1[i] = u[i]       # shift solutions

   if (it % nout == 0 or it == nt):                 # output every nout steps
      fname = "wave_{0:4.2f}.txt".format(t)
      out = open(fname,"w")
      out.write("t = {0:4.2f}\n".format(t))
      out.write("     x          u\n")
      for i in range(1,nx+1):
         out.write("{0:10.5f}{1:10.5f}\n".format(x[i],u[i]))
      out.close
