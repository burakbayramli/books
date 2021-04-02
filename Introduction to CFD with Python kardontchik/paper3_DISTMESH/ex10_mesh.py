"""
   Usage: run ex10_mesh
   ('fluid around an ellipse')
   Uniform mesh 
   CPU time = 172 sec
   qmin = 0.70, minimum angle = 36 degrees

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
# Rectangle vertices
x1 = -3; x2 = 5
y1 = -1; y2 = 5

# any fixed [xp,yp] points on the boundary?
pfix = np.zeros((0,2))   # null 2D array, no fixed points provided
pfix = np.array([ [-2,2], [4,2], [-3,-1], [5,-1], [-3,5], [5,5] ])

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -3; ymin = -1
xmax = 5; ymax = 5
h0 = 0.3

# define the distance function fd
fd_el = Ellipse(xc,yc,a,b)
fd_rect = Rectangle(x1,x2,y1,y2)
fd = Diff(fd_rect,fd_el)

# define the size function fh
fh = lambda p: np.ones(len(p))

start = clock()
p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4)
end = clock()
print 'CPU time = %g [sec] ' % (end-start)
tri,bbars = find_boundary(p,t,fh)
ext_bound_nodes,ext_bound,int_bound_nodes,int_bound = boundary_info(p,bbars)

