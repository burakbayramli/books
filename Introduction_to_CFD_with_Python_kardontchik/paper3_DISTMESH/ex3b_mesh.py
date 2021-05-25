"""
   Usage: run ex3b_mesh
   Persson, Fig 5.1, Ex 3b
   Non-uniform mesh on diff of a circle and a rectangle
   Note: an artificial sliver forms on the internal boundary:

List of triangles with q < 0.200 and the (x,y) location of their nodes

q     t[i]      t[nodes]         [x,y][0]       [x,y][1]       [x,y][2]
0.01   172  [ 162, 144, 160]     [+0.32,+0.23]  [+0.37,+0.15]  [+0.35,+0.19]

This sliver is eliminated by commenting out the additional code added
at the end, as explained in ex1b_mesh

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
h0 = 0.05

# any fixed [xp,yp] points on the boundary?
#pfix = np.zeros((0,2))   # null 2D array, no fixed points provided
pfix = np.array([ [-1,-1], [-1,1], [1,-1], [1,1] ])

# distance functions
fd_cir = Circle(xc,yc,r)
fd_rect = Rectangle(x1,x2,y1,y2)
fd = Diff(fd_rect,fd_cir)
# size function
#fh = lambda p: np.ones(len(p))
# fh = min(4*|p| - 1, 2)
fh = lambda p: np.minimum(4.0*np.sqrt(np.sum(p**2,1)) -1, 2)

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,Iflag=4)

tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)

"""
# --------------------------------------------------------------
#                       REPAIR THE MESH
# If mesh repair is needed due to the existence of spurious slivers,
# comment out the following code, enter the indices of the spurious
# triangles as a list, save the file and re-run again
# -------------------------------------------------------------

# enter the indices of the spurious triangles:
indexes = [172]
# --------------------------------------------
t = mesh_repair(p,t,fh,indexes)
# find the proper boundary
tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)
"""
