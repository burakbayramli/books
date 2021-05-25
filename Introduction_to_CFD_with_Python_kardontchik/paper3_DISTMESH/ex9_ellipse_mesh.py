"""
   Usage: run ex9_ellipse_mesh
   Triangulizes an ellipse
   Uniform mesh on ellipse
   CPU time = 160 sec
   qmin = 0.74, minimum angle = 35 degrees

"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
from scipy.optimize import fmin
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *

plt.close('all')

# Create the shape

# Ellipse parameters:
xc = 1.0; yc = 2.0
a = 3.0; b = 1.5

# any fixed [xp,yp] points on the boundary?
pfix = np.zeros((0,2))   # null 2D array, no fixed points provided


# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -2.5; ymin = 0
xmax = 4.5; ymax = 4
h0 = 0.3

# define the distance function fd
fd = Ellipse(xc,yc,a,b)

# define the size function fh
fh = lambda p: np.ones(len(p))

start = clock()
p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4)
end = clock()
print 'CPU time = %g [sec] ' % (end-start)

tri,bbars = find_boundary(p,t,fh)
boundary_nodes,boundary = boundary_info(p,bbars)

