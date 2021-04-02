"""
   Usage: run flat_mesh1_1x25
   angle of attack = 1.25
   flattens the triangular + body_fitted mesh
"""
# Save generated mesh in file:
filename = 'flat_mesh1_1x25deg'
# include any other useful info, for ex, attack angle: 'flat_mesh1_40deg'

import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *
from Meshing_Tools import *
from EMBED import *

from STRUCTUREDMESH import *

from QUAD_PROCESSING import *

from DEF_NACA0012 import *

plt.close('all')

# ----------------------------------------------------
#               TRIANGULAR MESH DATA
# ----------------------------------------------------
data = np.load('mesh1_1x25deg.npz')
h0 = data['h0']
p = data['p']
tri = data['tri']
bbars = data['bbars']
ext_bnodes = data['ext_bnodes']
ext_bbars = data['ext_bbars']
int_bnodes = data['int_bnodes']
int_bbars = data['int_bbars']

# ----------------------------------------------------
#           BODY_FITTED MESH DATA
# ----------------------------------------------------
data = np.load('qmesh1_1x25deg.npz')
N = data['N']
M = data['M']
qp = data['qnodes']
quad = data['quad']

# ----------------------------------------------------
#                   MESHES
# ----------------------------------------------------
# triangular plot
plt.figure()
tx = p[:,0]; ty = p[:,1]
for t in tri:
    t_i = [t[0],t[1],t[2],t[0]]
    plt.plot(tx[t_i],ty[t_i],'k')
    plt.hold('on')
plt.title('TRIANGULAR MESH')
plt.axis('equal')
plt.hold('off')
plt.show()

# body-fitted mesh
plt.figure()
qx = qp[:,0]; qy = qp[:,1]
for q in quad:
    q_i = [q[0],q[1],q[2],q[3],q[0]]
    plt.plot(qx[q_i],qy[q_i],'k')
    plt.hold('on')
plt.title('BODY-FITTED QUADRILATERAL MESH')
plt.axis('equal')
plt.hold('off')
plt.show()

# total mesh
plt.figure()
for t in tri:
    t_i = [t[0],t[1],t[2],t[0]]
    plt.plot(tx[t_i],ty[t_i],'k')
    plt.hold('on')
for q in quad:
    q_i = [q[0],q[1],q[2],q[3],q[0]]
    plt.plot(qx[q_i],qy[q_i],'k')
    plt.hold('on')
plt.title('TOTAL MESH')
plt.axis('equal')
plt.hold('off')
plt.show()

# flattened mesh

flat = FLATTEN(quad,qp,int_bnodes,tri,p)
new_p, new_tri = flat()


plt.figure()
xflat = new_p[:,0]; yflat = new_p[:,1]
for t in new_tri:
    t_i = [t[0],t[1],t[2],t[0]]
    plt.plot(xflat[t_i],yflat[t_i],'k')
    plt.hold('on')
plt.title('FLATTENED TRIANGULAR MESH')
plt.axis('equal')
plt.hold('off')
plt.show()

# find the boundary
bbars = boundary_bars(new_tri)
ext_bnodes,ext_bbars,int_bnodes,int_bbars =  \
                    boundary_info(new_p,bbars)

# Note: to impose the external boundary conditions the 4 nodes
# defining the corners of the wind tunnel will be new_p[N:N+4]
# Note: to impose the BC on the airfoil, since they are all reflective,
# we can take any two nodes of int_bnodes, for ex, int_bnodes[0], and
# int_bnodes[9] (or any number less than N)
# -----------------------------------------------------------
#                   save the mesh data
# -----------------------------------------------------------
# note: h0 should be the minimum triangle edge, that will be at
# the internal boundary. At the internal boundary the minimum edge
# length is about 0.2-0.1 of the original h0. However, this is only
# used in VISUAL_2D, so this inaccurate value should not affect the
# output of VISUAL_2D

np.savez('%s' %(filename), \
         h0 = h0, \
         p = new_p, \
         tri = new_tri, \
         bbars = bbars, \
         ext_bnodes = ext_bnodes, \
         ext_bbars = ext_bbars, \
         int_bnodes = int_bnodes, \
         int_bbars = int_bbars)


# to load use, for example:
# data = np.load('flat_mesh1_40deg.npz')
# h0 = data['h0']
# p = data['p']
# tri = data['tri']
# bbars = data['bbars']
# ext_bnodes = data['ext_bnodes']
# ext_bbars = data['ext_bbars']
# int_bnodes = data['int_bnodes']
# int_bbars = data['int_bbars']
