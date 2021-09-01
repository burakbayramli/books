# Steady-state temperature distribution in a thin conducting disk
from math import *
from pde import *

def Func(x, y):                                  # RHS function for Poisson2D
   return 0e0

#============================================================================
def BoundCondEllipse(u, imin, imax, nx, ny):
#----------------------------------------------------------------------------
#  Determines the boundary indexes imin[j] and imax[j] (j=1,ny) for an
#  elliptical domain, determined by the number of mesh points nx and ny, and
#  sets values on the boundaries for the function u
#----------------------------------------------------------------------------
   x0 = 0.5e0*(nx + 1e0)                    # center of dimensionless ellipse
   y0 = 0.5e0*(ny + 1e0)
   a = x0 - 1e0                                                   # semi-axes
   b = y0 - 1e0

   for j in range(1,ny+1):                                 # boundary indexes
      yj = j - y0                                     # relative y-coordinate
      xi = a * sqrt(1e0 - yj*yj/(b*b))              # x-coordinate on ellipse
      if (xi == 0e0): xi = 0.75e0           # correction for 1-point boundary

      imin[j] = int(x0 - xi + 0.5e0)                    # left boundary index
      imax[j] = int(x0 + xi + 0.5e0)                   # right boundary index
                                                            # boundary values
   for i in range(imin[1],imax[1]+1): u[i][1] = 0e0                  # bottom
   for j in range(2,ny):
      u[imin[j]][j] = 100e0                                            # left
      u[imax[j]][j] = 0e0                                             # right
   for i in range(imin[ny],imax[ny]+1): u[i][ny] = 0e0                  # top

# main

a = 5                                                           # disk radius
xmin = -a; xmax = a; ymin = -a; ymax = a                  # domain boundaries
nx = 51; ny = 51                                      # number of mesh points
eps = 1e-5                                      # relative solution tolerance

u = [[0]*(ny+1) for i in range(nx+1)]                              # solution
x = [0]*(nx+1); y = [0]*(ny+1)                       # mesh point coordinates
imin = [0]*(ny+1)                              # indexes of y-line boundaries
imax = [0]*(ny+1)

hx = (xmax-xmin)/(nx-1)
for i in range(1,nx+1): x[i] = xmin + (i-1)*hx                # x-mesh points
hy = (ymax-ymin)/(ny-1)
for j in range(1,ny+1): y[j] = ymin + (j-1)*hy;               # y-mesh points

for j in range(1,ny+1):                                 # initialize solution
   for i in range(1,nx+1): u[i][j] = -1e99

BoundCondEllipse(u,imin,imax,nx,ny)                         # boundary values

for j in range(2,ny):                 # initial approximation of the solution
   for i in range(imin[j]+1,imax[j]): u[i][j] = 0e0          # interior nodes

Poisson2D(u,x,y,imin,imax,nx,ny,eps,Func)

out = open("Poisson.txt","w")                              # open output file
out.write("      x         y          u\n")
for j in range(1,ny+1):
   for i in range(1,nx+1):
      out.write(("{0:10.5f}{1:10.5f}{2:14.5e}\n").format(x[i],y[j],u[i][j]))
out.close()
