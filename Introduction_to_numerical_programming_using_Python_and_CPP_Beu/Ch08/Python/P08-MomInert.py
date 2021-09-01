# Rotation of a molecule to the system of principal axes
from math import *
from eigsys import *
from coords import *
from matutil import *
from graphlib import *

n = 5                                                   # number of particles
m = [0,12.000, 1.000, 1.000, 1.000, 1.000]                           # masses
x = [0, 0.000, 0.635,-0.635,-0.635, 0.635]                        # positions
y = [0, 0.000, 0.635,-0.635, 0.635,-0.635]                     # [0] not used
z = [0, 0.000, 0.635, 0.635,-0.635,-0.635]
r = [0, 0.300, 0.200, 0.200, 0.200, 0.200]                            # radii
col = ["", "red", "blue", "blue", "blue", "gray"]                    # colors

dmax = 1.5e0                                      # cutoff distance for bonds

m[5] += 1e-10           # "mark" last particle to set principal symmetry axis

GraphInit(1200,600)

title = "Initial configuration"
PlotParticles(x,y,z,r,col,n,dmax,0e0,0e0,0,0e0,0e0,0,0.05,0.25,0.2,0.8,title)

MovetoCM(m,x,y,z,n)                                       # move to CM system

MomInert = PrincipalAxes(m,x,y,z,n,1)         # align main symmetry axis to x
print("Structure aligned to x-axis:")
VecPrint(x,n); VecPrint(y,n); VecPrint(z,n)
print("Principal moments of inertia:")
VecPrint(MomInert,3)
title = "System of principal axes - main x"
PlotParticles(x,y,z,r,col,n,dmax,0e0,0e0,0,0e0,0e0,0,0.45,0.65,0.2,0.8,title)

MomInert = PrincipalAxes(m,x,y,z,n,-1)        # align main symmetry axis to z
print("\nStructure aligned to z-axis:")
VecPrint(x,n); VecPrint(y,n); VecPrint(z,n)
print("Principal moments of inertia:")
VecPrint(MomInert,3)
title = "System of principal axes - main z"
PlotParticles(x,y,z,r,col,n,dmax,0e0,0e0,0,0e0,0e0,0,0.75,0.95,0.2,0.8,title)

MainLoop()
