"""
   Usage: run ex3a_mesh
   Persson, Fig 5.1, Ex 3a
   Uniform mesh on diff of a circle and a rectangle 
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

from DISTMESH import *

plt.close('all')

# Circle parameters:
# center at (xc,yc), radius r
xc = 0; yc = 0; r = 0.4
# Rectangle vertices:
x1,y1 = -1.0,-1.0
x2,y2 = 1.0,1.0

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -1.2; ymin = -1.2
xmax = 1.2; ymax = 1.2
h0 = 0.15

# distance functions
fd_cir = Circle(xc,yc,r)
fd_rect = Rectangle(x1,x2,y1,y2)
fd = Diff(fd_rect,fd_cir)
# size function
fh = lambda p: np.ones(len(p))

# any fixed [xp,yp] points on the boundary?
#pfix = np.zeros((0,2))   # null 2D array, no fixed points provided
pfix = np.array([ [-1,-1], [-1,1], [1,-1], [1,1] ])

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4)

tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)
