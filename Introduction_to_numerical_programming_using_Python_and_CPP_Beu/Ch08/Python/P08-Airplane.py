# Rotation of an airplane
from math import *
from eigsys import *
from coords import *
from matutil import *
from graphlib import *

n = 10                                              # number of corner points
m = [0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0]                                # masses
x = [0,-2,-6, 2, 0, 2, 6, 1, 2,-2, 2]                             # positions
y = [0,-2, 2,-6, 2, 0, 6, 2, 1,-2, 2]                          # [0] not used
z = [0, 0, 0, 0, 0, 0, 0, 1, 1, 4, 1]

n3 = 9                                         # number of defining triangles
ind1 = [0, 1, 1, 1, 1, 1, 1, 1, 6, 1]           # indexes of triangle corners
ind2 = [0, 2, 3, 4, 5, 4, 5, 7, 7, 9]
ind3 = [0, 4, 5, 6, 6, 7, 8, 8, 8,10]

GraphInit(800,800)

title = "Initial configuration"
PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
           0e0,0e0,0,0e0,0e0,0,0.05,0.45,0.55,0.92,title)

MovetoCM(m,x,y,z,n)                                    # move structure to CM

PrincipalAxes(m,x,y,z,n,1)                # rotate airplane to principal axes
print("Airplane in system of principal axes:")
VecPrint(x,6); VecPrint(y,6); VecPrint(z,6)
title = "System of principal axes"
PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
           0e0,0e0,0,0e0,0e0,0,0.55,0.95,0.55,0.92,title)

f = pi/180e0
phi = 20                                                                # yaw
cosp = cos(phi*f); sinp = sin(phi*f)
for i in range(1,n+1):                         # rotate airplane about z-axis
   xi = x[i]
   x[i] = cosp * xi - sinp * y[i]
   y[i] = sinp * xi + cosp * y[i]
print("\nRotated airplane yaw = {0:4.1f}".format(phi))
VecPrint(x,6); VecPrint(y,6); VecPrint(z,6)
title = "yaw = {0:4.1f}".format(phi)
PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
           0e0,0e0,0,0e0,0e0,0,0.05,0.45,0.05,0.42,title)

phi = -35                                                              # roll
cosp = cos(phi*f); sinp = sin(phi*f)
for i in range(1,n+1):                         # rotate airplane about x-axis
   yi = y[i]
   y[i] = cosp * yi - sinp * z[i]
   z[i] = sinp * yi + cosp * z[i]
print("\nRotated airplane roll = {0:4.1f}".format(phi))
VecPrint(x,6); VecPrint(y,6); VecPrint(z,6)
title = "roll = {0:4.1f}".format(phi)
PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
           0e0,0e0,0,0e0,0e0,0,0.55,0.95,0.05,0.42,title)

MainLoop()
