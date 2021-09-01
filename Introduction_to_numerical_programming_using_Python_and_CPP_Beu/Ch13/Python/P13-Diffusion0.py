# Solves the 1D diffusion equation by finite-difference schemes
from math import *
from pde import *
from graphlib import *

def Init(u, x, nx):             # initial solution for the diffusion equation
   global L                                        # extent of spatial domain
   for i in range(1,nx+1): u[i] = sin(pi*x[i]/L)

# main

D    = 0.1e0                                          # diffusion coefficient
L    = 1e0                                             # [0,L] spatial domain
nx   = 21                                     # number of spatial mesh points
tmax = 6.0e0                                               # propagation time
ht   = 1.25e-2                                                    # time step

u0 = [0]*(nx+1); u = [0]*(nx+1)                                    # solution 
x = [0]*(nx+1)                                                 # spatial mesh

hx = L/(nx-1)
for i in range(1,nx+1): x[i] = (i-1)*hx                        # spatial mesh

Init(u0,x,nx)                                              # initial solution

t = 0e0
while (t < tmax):                                             # temporal loop
   t += ht                                                    # increase time
   PropagFTCS(u0,u,nx,D,hx,ht)                           # propagate solution

   for i in range(1,nx): u0[i] = u[i]                       # shift solutions

out = open("diffusion.txt","w")                            # open output file
out.write(("lambda = {0:f} t = {1:f}\n").format(D*ht/(hx*hx),t))
out.write("      x         u       exact\n")
f = exp(-pi*pi*D*t/(L*L))
for i in range(1,nx+1):
   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n"). \
             format(x[i],u[i],f*sin(pi*x[i]/L)))
out.close()

GraphInit(800,600)
Plot(x,u,nx,"blue",1,0.15,0.95,0.15,0.85,"x","u","Diffusion")
MainLoop()
