# Solves the 2D Poisson equation in a rectangular domain
from math import *
from pde import *
from graphlib import *

def Func(x, y):                                   # RHS function for Poisson0
   return cos(x+y) - cos(x-y)

# main

xmin = -pi; xmax = pi; ymin = -pi; ymax = pi              # domain boundaries
nx = 51; ny = 51                                      # number of mesh points
eps = 1e-5                                      # relative solution tolerance

u = [[0]*(ny+1) for i in range(nx+1)]                              # solution
x = [0]*(nx+1); y = [0]*(ny+1)                       # mesh point coordinates

hx = (xmax-xmin)/(nx-1)
for i in range(1,nx+1): x[i] = xmin + (i-1)*hx                # x-mesh points
hy = (ymax-ymin)/(ny-1)
for j in range(1,ny+1): y[j] = ymin + (j-1)*hy                # y-mesh points

for j in range(1,ny+1):               # initial approximation of the solution
   for i in range(1,nx+1): u[i][j] = 0e0            # and boundary conditions

Poisson0(u,x,y,nx,ny,eps,Func)

out = open("Poisson.txt","w")                              # open output file
out.write("      x         y          u\n")
for j in range(1,ny+1):
   for i in range(1,nx+1):
      out.write(("{0:10.5f}{1:10.5f}{2:14.5e}\n").format(x[i],y[j],u[i][j]))
out.close()

umin = umax = u[1][1]                   # minimum and maximum of the solution
for j in range(1,ny+1):
   for i in range(1,nx+1):
      if (u[i][j] < umin): umin = u[i][j]
      if (u[i][j] > umax): umax = u[i][j]

GraphInit(800,800)
Contour(u,nx,ny,xmin,xmax,ymin,ymax,umin,umax, \
        0.15,0.85,0.15,0.85,"x","y","Poisson")
MainLoop()
