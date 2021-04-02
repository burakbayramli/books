"""
   Usage: run total_mesh
   triangular + one-sided body_fitted mesh
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt
from time import clock

from DISTMESH import *
from Meshing_Tools import *
from EMBED import *

from STRUCTUREDMESH import *

from DEF_NACA0012 import *

plt.close('all')

# ----------------------------------------------------
#               TRIANGULAR MESH DATA
# ----------------------------------------------------
data = np.load('tri_mesh_40deg.npz')
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
data = np.load('quad_mesh_40deg.npz')
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
