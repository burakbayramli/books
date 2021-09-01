# Solves the diffusion equation for spatially variable diffusion coefficient
from math import *
from pde import *
from graphlib import *

def Init(u, x, D, nx):                                       # initialization
   global L                                        # extent of spatial domain
   x1 = 0.4e0*L                                            # limit of layer 1
   x2 = 0.6e0*L                                            # limit of layer 2
   c = 0.5e0*(x1+x2)                                      # center of layer 2
   w = 0.5e0*(x2-x1)                                  # half-width of layer 2
   d = 0.01e0 * L                          # drop distance of switch function
   D1 = 0.1e0; D2 = 0.001e0           # nominal D coef. in layers 1 (3) and 2

   for i in range(1,nx+1):      # switch function: 1/0 inside/outside layer 2
      fsw = 1e0 / (1e0 + exp((fabs(x[i]-c)-w)/d))
      D[i] = D1 + (D2 - D1) * fsw
      u[i] = 1e0 if x[i] <= x1 else 0e0

# main

L      = 1e0                                           # [0,L] spatial domain
iopBC1 = 1; iopBC2 = 1                   # left/right boundary condition type
Jdiff1 = 1e0; Jdiff2 = 1e0                       # left/right boundary fluxes
nx     = 101                                  # number of spatial mesh points
tmax   = 2e0                                               # propagation time
ht     = 1e-3                                                     # time step
nout   = 100                                        # output every nout steps

nt = (int)(tmax/ht + 0.5)                              # number of time steps
u0 = [0]*(nx+1); u = [0]*(nx+1)                                    # solution 
x = [0]*(nx+1)                                                 # spatial mesh
D = [0]*(nx+1)                                         # difusion coefficient

hx = L/(nx-1)
for i in range(1,nx+1): x[i] = (i-1)*hx                        # spatial mesh

Init(u0,x,D,nx)             # initialization: diffusion coefficient, solution

GraphInit(1000,700)

out = open("diffusion.txt","w")
out.write("      x   ")
for i in range (1,nx+1): out.write("{0:10.5f}".format(x[i]))   # print x-mesh
out.write("\n")
out.write("      t         u\n")

for it in range(1,nt+1):                                          # time loop
   t = it*ht                                             # propagate solution
   PropagDiff(u0,u,D,nx,hx,ht,iopBC1,iopBC2,Jdiff1,Jdiff2)

   for i in range(1,nx+1): u0[i] = u[i]                     # shift solutions

   if (it % nout == 0 or it == nt):                 # output every nout steps
      out.write("{0:10.5f}".format(t))
      for i in range (1,nx+1): out.write("{0:10.5f}".format(u[i]))
      out.write("\n")

      GraphClear()
      title = "Diffusion  t = {0:4.2f}".format(t)
      Plot(x,u,nx,"blue",1,0.15,0.95,0.50,0.90,"None","u",title)
      Plot(x,D,nx,"red",1,0.15,0.95,0.08,0.48,"x","D","")
      GraphUpdate()
out.close

MainLoop()
