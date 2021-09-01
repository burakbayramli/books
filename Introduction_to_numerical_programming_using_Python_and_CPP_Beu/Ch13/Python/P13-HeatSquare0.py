# Steady-state temperature distribution in a thin square conducting plate
from math import *
from pde import *
from graphlib import *

def Func(x, y):                                  # RHS function for PoissonXY
   return 0e0

def CondX(y):                    # Coefficients for left and right boundaries
   alf_min = 1e0; bet_min = 0e0; gam_min = 10e0 * (10e0 - y)
   alf_max = 1e0; bet_max = 0e0; gam_max = 0e0

   return (alf_min, bet_min, gam_min, alf_max, bet_max, gam_max)

def CondY(x):                   # Coefficients for lower and upper boundaries
   alf_min = 1e0; bet_min = 0e0; gam_min = 10e0 * (10e0 - x)
   alf_max = 1e0; bet_max = 0e0; gam_max = 0e0

   return (alf_min, bet_min, gam_min, alf_max, bet_max, gam_max)

# main

xmin = 0e0; xmax = 10e0; ymin = 0e0; ymax = 10e0          # domain boundaries
nx = 41; ny = 41                                      # number of mesh points
eps = 1e-5                                      # relative solution tolerance

u = [[0]*(ny+1) for i in range(nx+1)]                              # solution
x = [0]*(nx+1); y = [0]*(ny+1)                       # mesh point coordinates

hx = (xmax-xmin)/(nx-1)
for i in range(1,nx+1): x[i] = xmin + (i-1)*hx                # x-mesh points
hy = (ymax-ymin)/(ny-1)
for j in range(1,ny+1): y[j] = ymin + (j-1)*hy                # y-mesh points

for j in range(1,ny+1):               # initial approximation of the solution
   for i in range(1,nx+1): u[i][j] = 0e0

PoissonXY(u,x,y,nx,ny,eps,Func,CondX,CondY)

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
        0.15,0.85,0.15,0.85,"x","y","Plate temperature")
MainLoop()
