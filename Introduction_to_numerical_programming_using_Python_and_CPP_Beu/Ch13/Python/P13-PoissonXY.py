# Solves the 2D Poisson equation in a rectangular domain
from math import *
from pde import *

def Func(x, y):                                  # RHS function for PoissonXY
   return cos(x+y) - cos(x-y)

def CondX(y):                    # Coefficients for left and right boundaries
   alf_min = 1e0; bet_min = 0e0; gam_min = 0e0
   alf_max = 1e0; bet_max = 0e0; gam_max = 0e0
   return (alf_min, bet_min, gam_min, alf_max, bet_max, gam_max)

def CondY(x):                   # Coefficients for lower and upper boundaries
   alf_min = 1e0; bet_min = 0e0; gam_min = 0e0
   alf_max = 1e0; bet_max = 0e0; gam_max = 0e0
   return (alf_min, bet_min, gam_min, alf_max, bet_max, gam_max)

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
   for i in range(1,nx+1): u[i][j] = 0e0

PoissonXY(u,x,y,nx,ny,eps,Func,CondX,CondY)

out = open("Poisson.txt","w")                              # open output file
out.write("      x         y          u\n")
for j in range(1,ny+1):
   for i in range(1,nx+1):
      out.write(("{0:10.5f}{1:10.5f}{2:14.5e}\n").format(x[i],y[j],u[i][j]))
out.close()
