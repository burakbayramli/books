"""
   Usage: run ex5a_mesh
   Shape as per Persson, Fig 5.1, Example (5)
   Different size function fh
   Ref: Persson, Fig 5.1, Example 5

"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

from DISTMESH import *

plt.close('all')
# Create the shape

# First Circle parameters:
# center at (xc,yc), radius r
xc1 = 0; yc1 = 0; r1 = 1

# Second Circle parameters:
# center at (xc,yc), radius r
xc2 = -0.4; yc2 = 0; r2 = 0.55

# y > 0 half-plane

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -1; ymin = 0
xmax = 1; ymax = 1
h0 = 0.05/3
# 0.05 is the minimum width of the region, found near x = -1,
# so we want to put at least 3 grid points there

# any fixed points to add?
#pfix = np.zeros((0,2))   # null array, no fixed points provided0,
pfix = np.array([ [-1,0], [1,0], [xc2 - r2,0], [xc2 + r2,0] ])

# define the distance function fd
fd_cir1 = Circle(xc1,yc1,r1)
fd_cir2 = Circle(xc2,yc2,r2)
fd_diff = Diff(fd_cir1,fd_cir2)
# add the intersect with the y > 0 plane
fd_upper_half_plane = lambda p: -p[:,1]
fd = Intersect(fd_diff,fd_upper_half_plane)

# define the size function fh
# use 1st the uniform distribution to check that we got the correct region
#fh = lambda p: np.ones(len(p))

# pcrit: coordinates (at the left corner) in the middle point between
# the two circles that almost touch each other xcrit = -0.975
xcrit = 0.5*((xc1-r1) + (xc2 - r2))
pcrit = np.array([xcrit,0])
# x AND y coordinates needed
# otherwise p - pcrit would mean [p[:,0] - xcrit, p[:,1] - xcrit]

#fh = lambda p: 1.0 + 4.0* np.sum((p-pcrit)**2, axis=1)

# fh at the critical point, (-0.95,0), equals 1
# fh at (1,0) equals ~17

# However, ... this is not enough
# It seems that the problematic triangles are being generated in
# the region around (-0.5,0.75). Let us reduce the density of points there
xc3 = xc2; yc3 = 0.5*(r1+r2); pc3 = np.array([xc3,yc3])
a3 = 4; b3 = 0.2
# we quadruple the size (a3 = 4) near pc3. The region of influence of fh2
# is within a distance of about b3 from pc3
#fh_add = lambda p: a3*np.exp(- np.sum((p - pc3)**2,axis=1)/b3**2)

fh = lambda p: 1.0 + 4.0* np.sum((p-pcrit)**2, axis=1) + \
     a3*np.exp(- np.sum((p - pc3)**2,axis=1)/b3**2)
     

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,Iflag=4,qmin=0.68)

tri,bbars = find_boundary(p,t,fh)
boundary_nodes,boundary = boundary_info(p,bbars)
