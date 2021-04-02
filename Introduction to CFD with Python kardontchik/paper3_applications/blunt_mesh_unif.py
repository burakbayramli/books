"""
   Usage: run blunt_mesh_unif
   (uniform grid)
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
from scipy.optimize import fmin
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *
from Meshing_Tools import *

from DEF_BLUNT_NOSE import *

plt.close('all')

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -2.5; ymin = -0.5
xmax = 2.5; ymax = 3.0

# h0 = 1.0/7.0 # ~ 0.14; Anderson used quadrilateral cells of length 1/7

h0 = 0.1 # dptol=0.005;CPU=411 sec;qmin=0.7;min_angle=35, 1064 triangles
qmin = 0.7

# define the size function fh
fh = lambda p: np.ones(len(p))

# any fixed [xp,yp] points on the boundary?
xm = 2.0
ym = ((xm + 1.0)/a)**(1.0/3.0)

v0 = [-2,0]
v1 = [-1,0]
v2 = [xm,ym]
v3 = [2,2.4]
v4 = [-2,2.4]

#pfix = np.zeros((0,2))   # null 2D array, no fixed points provided
pfix = np.array([v0,v1,v2,v3,v4])

start = clock()
p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4,qmin=qmin)
end = clock()
print 'CPU time = %g [sec] ' % (end-start)

tri,bbars = find_boundary(p,t,fh)
boundary_nodes,boundary_bars = boundary_info(p,bbars)

# -----------------------------------------------------------
#                   save the mesh data
# -----------------------------------------------------------
np.savez('blunt_mesh_unif_h0_0x1', \
         h0 = h0, \
         p = p, \
         tri = tri, \
         bbars = bbars, \
         boundary_nodes = boundary_nodes, \
         boundary_bars = boundary_bars)

# to load use:
# data = np.load('blunt_mesh_unif_h0_0x1.npz')
# h0 = data['h0']
# p = data['p']
# tri = data['tri']
# bbars = data['bbars']
# boundary_nodes = data['boundary_nodes']
# boundary_bars = data['boundary_bars']
