"""
   Usage: run mesh1_1x25def
"""
# Save generated mesh in file:
filename = 'mesh1_1x25deg' # include any other useful info, for ex: 'mesh1_1x25deg'

import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *
from Meshing_Tools import *
from EMBED import *

from DEF_NACA0012 import *

plt.close('all')
        
# ----------------------------------------------------
#           AIRFOIL SHAPE -X
# ----------------------------------------------------
xairfoil_verts = cverts[0:-1]  # non-closed polygon
# (eliminates the two points too close to the tail)
airfoil_verts = np.zeros((len(xairfoil_verts)-2,2))
airfoil_verts[0] = xairfoil_verts[0]
airfoil_verts[1:] = xairfoil_verts[2:-1]
cverts = np.concatenate((airfoil_verts,[airfoil_verts[0]]))
# ---------------------------------------------------
#           ELLIPSE SHAPE
# ---------------------------------------------------
# ellipse parameters
xc = 0.5; yc = 0.0
a = 0.7; b = 0.3
# number of points on the perimeter
N = len(airfoil_verts)

f1 = Uniform_Embed(xc,yc,a,b,N)
hh0,ellipse_verts = f1()

# ---------------------------------------------------
#           ANGLE OF ATTACK
# ---------------------------------------------------
angle = 1.25 
ang = - angle*(pi/180.0)
grot = Protate_About(ang,xc,yc)
airfoil_verts = grot(airfoil_verts)
ellipse_verts = grot(ellipse_verts)

plt.figure(1)
plt.plot(airfoil_verts[:,0],airfoil_verts[:,1],'ro', \
         ellipse_verts[:,0],ellipse_verts[:,1],'b*')
plt.title('AIRFOIL EMBEDDED IN ELLIPSE; ATTACK ANGLE = %g' %(angle))
plt.axis('equal')
plt.grid()
plt.show()

# ---------------------------------------------------
#               MESH
# ---------------------------------------------------
# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -2; ymin = -1.5
xmax = 3; ymax = 1.5
# granularity
# h0 = 1.7*hh0  # to avoid adding more points on the ellipse
h0 = 1.4*hh0
qmin = 0.68
# angle = 1.25, run with qmin = 0.68
# stopped at iter # 41 with:qmin=0.68,min_angle=33; 1855 tri;5 ears, corrected
# CPU = 220 sec

# Define the distance function
# External rectangle
x1 = -2; x2 = 3
y1 = -1.5; y2 = 1.5
# Define the distance function fd
fd_rect = Rectangle(x1,x2,y1,y2)
fd_poly = Polygon(ellipse_verts)
fd = Diff(fd_rect,fd_poly)

# define the size function fh
#fh = lambda p: np.ones(len(p))
def fh(p):
    xc = 0.5; yc = 0.0
    a = 0.7   # major semi-axis of ellipse
    hh0 = 0.0545  # from ellipse calculations
    rmin = 1.2*a
    #hmin = 1.7*hh0 # to avoid adding more points on the ellipse
    hmin = 1.4*hh0 
    fh1 = hmin*np.sqrt(((p[:,0] - xc)**2 + (p[:,1] - yc)**2)/rmin**2)
    fh2 = hmin
    # for r < rmin use hmin
    # for r > rmin increase the size
    hsize = np.maximum(fh1,fh2)
    return hsize

# define fd_embed
fd_embed = Embed_Dist(ellipse_verts)

# any fixed points to add?
# pfix = np.zeros((0,2))  # no fixed points
pfix1 = ellipse_verts
pfix2 = np.array([ [x1,y1], [x2,y1], [x2,y2], [x1,y2] ])
pfix = np.concatenate((pfix1,pfix2))

start = clock()
p,t,bars = \
    distmesh_embed(fd_embed,fd,fh,h0,xmin,ymin,xmax,ymax,pfix,dptol=0.005,Iflag=4,qmin=qmin)
end = clock()
print 'CPU time = %g [sec] ' % (end-start)

tri,bbars = find_boundary(p,t,fh)
ext_bnodes,ext_bbars,int_bnodes,int_bbars =  \
                    boundary_info(p,bbars)

# delete any 'pointed ears' in the internal boundary
fears = Ears(N,int_bnodes,int_bbars,tri,p)
int_bnodes,int_bbars,tri = fears()

# -----------------------------------------------------------
#                   save the mesh data
# -----------------------------------------------------------
np.savez('%s' %(filename), \
         h0 = h0, \
         p = p, \
         tri = tri, \
         bbars = bbars, \
         ext_bnodes = ext_bnodes, \
         ext_bbars = ext_bbars, \
         int_bnodes = int_bnodes, \
         int_bbars = int_bbars)


# to load use:
# data = np.load('mesh3.npz')
# h0 = data['h0']
# p = data['p']
# tri = data['tri']
# bbars = data['bbars']
# ext_bnodes = data['ext_bnodes']
# ext_bbars = data['ext_bbars']
# int_bnodes = data['int_bnodes']
# int_bbars = data['int_bbars']

