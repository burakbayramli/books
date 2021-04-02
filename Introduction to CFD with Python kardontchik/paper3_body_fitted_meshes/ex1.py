"""
   Usage: run ex1
   Composite Mesh example: Unstructured (triangular) + Structured (rectangular)
"""
import numpy as np
from numpy import pi as pi
from scipy.spatial import Delaunay
import matplotlib.pylab as plt

from DISTMESH import *
from Meshing_Tools import *
from STRUCTUREDMESH import *

plt.close('all')

# Circle parameters:
# center at (xc,yc), radius r
xc = 0; yc = 0; r = 0.4 
# external Rectangle vertices:
x1,y1 = -1.0,-1.0
x2,y2 = 1.5,1.0
# internal Rectangle vertices
x3 = xc; y3 = -r
x4 = x2; y4 = r

# Define the region in (x,y) plane in which we will create the mesh
# and the grid granularity
xmin = -1.2; ymin = -1.2
xmax = 1.7; ymax = 1.2
h0 = 0.1

# distance functions
fd_cir = Circle(xc,yc,r)
fd_ext_rect = Rectangle(x1,x2,y1,y2)
fd_int_rect = Rectangle(x3,x4,y3,y4)

fd1 = Union(fd_cir,fd_int_rect)
fd = Diff(fd_ext_rect,fd1)

# size function
fh = lambda p: np.ones(len(p))

# any fixed [xp,yp] points on the boundary?
#pfix = np.zeros((0,2))   # null 2D array, no fixed points provided

pfix = np.array([ [-1,-1], [1.5,-1], [1.5,-r], [1.5,r], [1.5,1], [-1,1] ])

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,qmin=0.72,Iflag=4)

tri,bbars = find_boundary(p,t,fh)
boundary_nodes,boundary_bars = boundary_info(p,bbars)

from_index = list(boundary_nodes).index(2)
to_index = list(boundary_nodes).index(3)

print ''
print 'total number of nodes in upper boundary-fitted structured mesh = %d' \
      % (to_index + 1 - from_index)

# ----------------------------------------------------
#   Define the p_top and p_bottom arrays
# ----------------------------------------------------

N = to_index + 1 - from_index

# node coordinates of top and bottom structured surface:
p_top = np.zeros((N,2))
p_bottom = np.zeros((N,2))
# node coordinates of top structured mesh surface:
p_top = p[boundary_nodes[from_index:to_index+1]]
# node coordinates of bottom structured mesh surface
# blunt nose cylinder: radius of circular nose = 0.25
# lower cylinder face
p_bottom[0:15,0] = 1.5 - h0*np.arange(15)
p_bottom[0:15,1] = -0.25*np.ones(15)
# upper cylinder face
p_bottom[28:,0] = h0 + h0*np.arange(15)
p_bottom[28:,1] = 0.25*np.ones(15)
# blunt circular nose
t = np.arange(13) # 13 points on semicircle (blunt nose)
p_bottom[15:28,0] = -0.25*np.sin((pi/12.0)*t)
p_bottom[15:28,1] = -0.25*np.cos((pi/12.0)*t)

# -----------------------------------------------
#       BUILD THE STRUCTURED MESH
# -----------------------------------------------
# M: mesh points between the body surface and the unstructured mesh
M = 6

# (epsilon,eta): [0,N]x[0,M]
epsilon = np.linspace(0,N-1,N)
eta = np.linspace(0,M-1,M)

x = np.zeros((N,M))
y = np.zeros((N,M))

f = UNIFORM(p_top,p_bottom)
x,y = f(M)

# visualize the structured mesh
plt.figure(4)
for i in range(M):
    plt.plot(x[:,i],y[:,i],'k')
    plt.hold('on')
for j in range(N):
    plt.plot(x[j,:],y[j,:],'k')
    plt.hold('on')
plt.title('MESH WITH LINEAR MAPPING')
plt.axis('equal')
plt.xlabel('x')
plt.ylabel('y')
plt.hold('off')
plt.show()

# ---------------------------------------------------------
#       ktrimesh routine using hold on plot
# ---------------------------------------------------------
def composite_ktrimesh(p,bars):
    # create the (x,y) data for the plot
    xx1 = p[bars[:,0],0]; yy1 = p[bars[:,0],1]
    xx2 = p[bars[:,1],0]; yy2 = p[bars[:,1],1]
    xmin = np.min(p[:,0])
    xmax = np.max(p[:,0])
    ymin = np.min(p[:,1])
    ymax = np.max(p[:,1])
    xmin = xmin - 0.05*(xmax - xmin)
    xmax = xmax + 0.05*(xmax - xmin)
    ymin = ymin - 0.05*(ymax - ymin)
    ymax = ymax + 0.05*(ymax - ymin)

    plt.figure()
    for i in range(len(xx1)):
        xp = np.array([xx1[i],xx2[i]])
        yp = np.array([yy1[i],yy2[i]])
        plt.plot(xmin,ymin,'.',xmax,ymax,'.',markersize=0.1)
        plt.plot(xp,yp,'k')
        plt.hold('on')
    plt.axis('equal')
    plt.title('MESH FOR FLOW AROUND BLUNT-NOSED SLAB')
    plt.xlabel('x')
    plt.ylabel('y')
    #plt.hold('off')
    #plt.show()
    return 1
# ------------------------------------------------------------
composite_ktrimesh(p,bars)
for i in range(M):
    plt.plot(x[:,i],y[:,i],'k')
    plt.hold('on')
for j in range(N):
    plt.plot(x[j,:],y[j,:],'k')
    plt.hold('on')
plt.axis('equal')
plt.xlabel('x')
plt.ylabel('y')
plt.hold('off')
plt.show()
