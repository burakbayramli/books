"""
   Usage: run qmesh1_1x25deg
   Uses one-sided body_fitted mesh
   quadrilateral mesh
"""
# Save generated mesh in file:
filename = 'qmesh1_1x25deg' # include any other useful info, for ex: 'qmesh1_1x25deg'

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
#           AIRFOIL SHAPE - X
# ----------------------------------------------------
xairfoil_verts = cverts[0:-1]  # non-closed polygon
# (eliminates two points too close to the tail)
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
angle = 1.25 # 40.0 
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
#               Q-MESH
# ---------------------------------------------------
# find minimum distance between nodes on the airfoil
dair = np.sqrt((cverts[1:,0] - cverts[0:-1,0])**2 + \
               (cverts[1:,1] - cverts[0:-1,1])**2)
plt.figure()
plt.plot(dair,'o')
plt.grid()
plt.title('DISTANCE BETWEEN CONSECUTIVE AIRFOIL NODES')
plt.show()
max_dair = np.max(dair)
min_dair = np.min(dair)
print 'max dist between airfoil nodes = %g' % (max_dair)
print 'min dist between airfoil nodes = %g' % (min_dair)
dist_ratio = hh0/min_dair
print 'max to min dist_ratio on airfoil = %g' % (dist_ratio)
# ----------------------------------
#
# ----------------------------------
# laminar edge increase per layer
# number of needed layers M = 19 in qmesh1 (uniform)
M = 10
beta = 1.2
f2 = ONE_SIDED(ellipse_verts,airfoil_verts)
x,y = f2(M,beta=beta)

# --------------------------------------
#       Quadrilateral node generation
# --------------------------------------
qp = np.zeros((N,M,2))
for i in range(N):
    qp[i,:,0] = x[i,:]
    qp[i,:,1] = y[i,:]

# Note: qp includes both the airfoil nodes and the ellipse nodes
# qp[0,0,:] = airfoil_verts[0]
# qp[0,-1,:] = ellipse_verts[0]
# qp[-1,0,:] = airfoil_verts[-1]
# qp[-1,-1,:] = ellipse_verts[-1]

plt.figure()
for i in range(N):
    for j in range(M):
        plt.plot(qp[i,j,0],qp[i,j,1],'ko')
        plt.hold('on')
plt.title('BODY-FITTED GRID NODES, ONE-SIDED, BETA = %g' %(beta))
plt.hold('off')
plt.show()

# Note: nice for short, efficient code, but we should flatten the
# quad nodes, so that, qp_flat[i] = np.array(x[i],y[i]), i=0,1,...N*M
qp_flat = np.zeros((N*M,2))
k = 0
for k in range(M):
    # by layer: nodes in 1st layer, followed by nodes in 2nd layer, ...
    # beginning from the tail, going around the upper surface till
    # the head and continuing around the lower surface till the last
    # node before the tail
    qp_flat[N*k: N*(k+1),0] = qp[:,k,0]
    qp_flat[N*k: N*(k+1),1] = qp[:,k,1]

# ------------------------------------------------------
#       Quadrilateral (ccw) generation (for Euler eqs)
# ------------------------------------------------------
# number of quads
num_quads = N*(M-1)   # N (not (N-1))because we have to close the contour
# quads nodes
quad = np.zeros((num_quads,4),dtype = int)
for k in range(M-1):
    # for fixed k, that is for fixed 'height above the airfoil
    # order the quads by layer, so quad[0:N] are the quads abutted
    # to the airfoil and quad[num_quads-N:] are the quads abutted
    # to the ellipse
    # This way it will be easier to apply the BC
    for i in range(N):
        # indexes n1,n2,... should correspond to the qp_flat[i]
        # qp_flat[i], with i = N*k + i, i = 0, 1,..., (N-1)
        n1 = N*k + i         # origin of the quadrilateral
        n2 = N*(k+1) + i     # next layer (same column, specified by i)
        if i < N-1:
            n3 = N*(k+1) + (i+1) # next layer, next column
            n4 = N*k + (i+1)     # present layer, next column
        else:   # close the contour
            n3 = N*(k+1)
            n4 = N*k
        quad[N*k+i]= np.array([ [n1,n2,n3,n4] ])
        


plt.figure()
x = qp_flat[:,0]; y = qp_flat[:,1]
for q in quad:
    q_i = [q[0],q[1],q[2],q[3],q[0]]
    plt.plot(x[q_i],y[q_i],'k')
    plt.hold('on')
plt.title('BODY-FITTED QUADRILATERAL MESH, ONE-SIDED, BETA = %g' % (beta))
plt.hold('off')
plt.show()    

print ''
print 'N = %g, M = %g, beta = %g, # of quads = %g' \
      % (N,M,beta,len(quad))

# -----------------------------------------------------------
#                   save the mesh data
# -----------------------------------------------------------
# N = number of nodes per layer, beginning from the tail, going
#    around the upper surface till the head an then continuing
#    around the lower surface 
# M = number of layers; 0 = layer on airfoil, (M-1) = layer on ellipse

np.savez('%s' %(filename), \
         N = N, \
         M = M, \
         qnodes = qp_flat, \
         quad = quad)

# to load use:
# data = np.load('qmesh2.npz')
# N = data['N']
# M = data['M']
# qnodes = data['qnodes']
# quad = data['quad']
