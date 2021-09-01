# Steady-state temperature distribution in a thin triangular conducting plate
from math import *
from pde import *
from graphlib import *

def Func(x, y):                                  # RHS function for Poisson2D
   return 0e0

#============================================================================
def BoundCondTriangle(u, imin, imax, nx, ny, iapex):
#----------------------------------------------------------------------------
#  Determines the boundary indexes imin[j] and imax[j] (j=1,ny) for a
#  triangular domain, determined by the number of mesh points nx, ny, and the
#  apex i-index iapex, and sets values on the boundaries for the function u
#----------------------------------------------------------------------------
   amin = (iapex-1e0)/(ny-1e0)                          # slope for left side
   amax = -(nx-iapex)/(ny-1e0)                         # slope for right side

   for j in range(1,ny+1):                           # determine line-indexes
      imin[j] = int(amin*(j-1) + 1e0 + 0.5e0)           # left boundary index
      imax[j] = int(amax*(j-1) + nx + 0.5e0)           # right boundary index
                                                            # boundary values
   for i in range(imin[1],imax[1]+1): u[i][1] = 100e0                # bottom
   for j in range(2,ny): u[imin[j]][j] = u[imax[j]][j] = 0e0   # left & right
   for i in range(imin[ny],imax[ny]+1): u[i][ny] = 0e0                  # top

# main

xmin = 0e0; xmax = 10e0; ymin = 0e0; ymax = 8e0           # domain boundaries
nx = 51; ny = 41                                      # number of mesh points
iapex = 26                                         # i-index of triangle apex
eps = 1e-5                                      # relative solution tolerance

u = [[0]*(ny+1) for i in range(nx+1)]                              # solution
x = [0]*(nx+1); y = [0]*(ny+1)                       # mesh point coordinates
imin = [0]*(ny+1)                              # indexes of y-line boundaries
imax = [0]*(ny+1)

hx = (xmax-xmin)/(nx-1)
for i in range(1,nx+1): x[i] = xmin + (i-1)*hx                # x-mesh points
hy = (ymax-ymin)/(ny-1)
for j in range(1,ny+1): y[j] = ymin + (j-1)*hy                # y-mesh points

for j in range(1,ny+1):                                 # initialize solution
   for i in range(1,nx+1): u[i][j] = -1e99

BoundCondTriangle(u,imin,imax,nx,ny,iapex)                  # boundary values

for j in range(2,ny):                 # initial approximation of the solution
   for i in range(imin[j]+1,imax[j]): u[i][j] = 0e0          # interior nodes

Poisson2D(u,x,y,imin,imax,nx,ny,eps,Func)

out = open("Poisson.txt","w")                              # open output file
out.write("      x         y          u\n")
for j in range(1,ny+1):
   for i in range(1,nx+1):
      out.write(("{0:10.5f}{1:10.5f}{2:14.5e}\n").format(x[i],y[j],u[i][j]))
out.close()

umin = umax = u[imin[1]][1]             # minimum and maximum of the solution
for j in range(1,ny+1):
   for i in range(imin[j],imax[j]+1):
      if (u[i][j] < umin): umin = u[i][j]
      if (u[i][j] > umax): umax = u[i][j]

GraphInit(800,800)
Contour(u,nx,ny,xmin,xmax,ymin,ymax,umin,umax, \
        0.15,0.85,0.15,0.85,"x","y","Plate temperature")
MainLoop()
