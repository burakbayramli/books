"""
   Usage: run ex2_mesh
   Persson, Fig 5.1, Ex 2
   Uniform mesh on diff of two circles

"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

from DISTMESH import *

plt.close('all')

# Create the shape

# External circle parameters:
xc1 = 0; yc1 = 0; r1 = 1 

# Internal circle parameters
xc2 = 0; yc2 = 0; r2 = 0.4


# any fixed [xp,yp] points on the boundary?
pfix = np.zeros((0,2))   # null 2D array, no fixed points provided


# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -1.0; ymin = -1.0
xmax = 1.0; ymax = 1.0
h0 = 0.1

# define the distance function fd
fd_cir1 = Circle(xc1,yc1,r1) 
fd_cir2 = Circle(xc2,yc2,r2)
fd = Diff(fd_cir1,fd_cir2)

# define the size function fh
fh = lambda p: np.ones(len(p))

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4)
# with default dptol = 0.001 it took 33 Delaunay triangulations to finish
# the final quality was q > 0.75 and min_angle > 37
# with dptol = 0.005 it took the same number of Delay triangulations (but,
# of course, ran faster) and the final quality was q > 0.80 and
# min_angle > 37

tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)

