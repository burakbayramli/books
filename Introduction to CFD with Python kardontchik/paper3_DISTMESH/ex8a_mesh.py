"""
   Usage: run ex8a_mesh
   Persson, Fig 5.1, Ex 8
   Uses uniform size function
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

from DISTMESH import *

plt.close('all')

# Create the shape
# Circle parameters:
# center at (xc,yc), radius r
xc1 = 0; yc1 = 0; r1 = 3

# Circle parameters:
# center at (xc,yc), radius r
xc2 = 1.75; yc2 = 0; r2 = 0.25

# Polygon vertices
verts1 = np.array([ [0,0], [2.8901,-0.79378], [4,0], [2.8901,0.79378] ])

# Polygon vertices
verts2 = np.array([ [2.5,0], [2.98339,-0.24671], [3.5,0], [2.98339,0.24671] ])

# any fixed [xp,yp] points on the boundary?
# pfix = np.zeros((0,2))   # null 2D array, no fixed points provided
pfix = np.array([ [0,0], [2.8901,-0.79378], [2.98339,-0.24671], \
                  [2.5,0], [2.98339,0.24671], [2.8901,0.79378] ])

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = 0; ymin = -1.0
xmax = 3; ymax = 1.0
h0 = 0.1

# define the distance function fd
# define the distance function fd
fd_cir1 = Circle(xc1,yc1,r1)
fd_cir2 = Circle(xc2,yc2,r2)
fd_poly1 = Polygon(verts1)
fd_poly2 = Polygon(verts2)
fd = Diff(Diff(Intersect(fd_cir1,fd_poly1),fd_cir2),fd_poly2)
# define the size function fh
# use 1st the uniform distribution to check that we got the correct region
fh = lambda p: np.ones(len(p))

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,Iflag=4)

tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)
