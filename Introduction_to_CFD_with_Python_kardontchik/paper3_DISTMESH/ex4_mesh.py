"""
   Usage: run ex4_mesh
   Persson, Fig 5.1, Ex 4
   Uniform mesh on diff of two polygons

"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

from DISTMESH import *

plt.close('all')

# Create the shape

# External polygon parameters:
phi = (2*pi/6)*np.array([0,1,2,3,4,5])
verts1 = np.zeros((6,2))
verts1[:,0], verts1[:,1] = np.cos(phi), np.sin(phi)

# Internal polygon parameters
rotation_angle = pi/6
f = Protate(rotation_angle)
verts2 = 0.5*f(verts1)

# any fixed [xp,yp] points on the boundary?
# pfix = np.zeros((0,2))   # null 2D array, no fixed points provided
pfix = np.concatenate((verts1,verts2), axis=0)

# runs also fast and good with pfix specifying only the verts of the
# external polygon, but the internal boundary is not sharp. I believe
# Parsson fixed also the vertices of the internal polygon

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -1.0; ymin = -1.0
xmax = 1.0; ymax = 1.0
h0 = 0.1

# define the distance function fd
fd_hexagon1 = Polygon(verts1)
fd_hexagon2 = Polygon(verts2)
fd = Diff(fd_hexagon1,fd_hexagon2)

# define the size function fh
# use 1st the uniform distribution to check that we got the correct region
fh = lambda p: np.ones(len(p))

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4)

tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)


