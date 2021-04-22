# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Square domain with partial temperature boundary condition
and partial insulated boundary condition.  Finite element mesh
that consists of four-node quadrilaterals.
 """

import math
from numpy import array
from numpy import zeros
from numpy import ones
from numpy import arange
from numpy import linspace
from numpy import unique
from numpy import dot
from numpy import linalg
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

##
# Evaluate for a given data
kappa = 0.2 # thermal conductivity
Q = 0.005 # uniform heat source
sidel = 48 # length of the side of the square domain
nes = 18 # number of elements per side
Dz = 1.0  # thickness of the slice

xs = linspace(0, sidel, nes+1)
ys = linspace(0, sidel, nes+1)
nx = len(xs) - 1
ny = len(ys) - 1

nnodes = (nx+1) * (ny+1)
nfes = nx * ny

# Generate the nodes
x = zeros((nnodes, 2)) # Coordinates of nodes: initialized to all zeros
k = 0
for j  in arange(0, (ny+1)):
    for i  in arange(0, (nx+1)):
        x[k, :] = [xs[i], ys[j]]
        k = k + 1

# Generate the elements
conn = zeros((nfes, 4), dtype=int)
k = 0
for i  in arange(0, (nx)):
    for j  in arange(0, (ny)):
        f = int(j * (nx+1) + i) + 1
        conn[k, :] = [f, f+1, f+(nx+1)+1, f+(nx+1)]
        k = k + 1

# Define the degrees of freedom
# Fixed temperature at the nodes on the boundary, except for a few
# in the lower-left corner
Fixed = [int(i)+1 for i in arange(nx/2, nx+1)]
Fixed = Fixed + [int(j*(nx+1))+1 for j in arange(ny/2, ny+1)]
Fixed = Fixed + [int(i+ny*(nx+1))+1 for i in arange(0, nx+1)]
Fixed = Fixed + [int(j*(nx+1)+ny)+1 for j in arange(0, ny+1)]
Fixed = unique(Fixed)

dof = zeros((nnodes,1), dtype=int)
k = nnodes-len(Fixed)
for index  in Fixed:
    dof[index-1] = k+1
    k = k + 1
k = 0    
for index  in range(0, nnodes):
    if (dof[index] == 0): 
        dof[index] = k+1
        k = k + 1
N_f = k
    
def gradNpar(xi, eta):
    """
    A one-liner to calculate the matrix of the basis function gradients
    in the parametric coordinates.
    """
    return array([
        [eta / 4 - 1. / 4, xi / 4 - 1. / 4],
        [1. / 4 - eta / 4, - xi / 4 - 1. / 4],
        [eta / 4 + 1. / 4, xi / 4 + 1. / 4],
        [- eta / 4 - 1. / 4, 1. / 4 - xi / 4]])
 
def N(xi, eta):
    """
    A one-liner to calculate the matrix of the basis function values.
    """
    return array([(xi - 1) * (eta - 1) / 4,
                  (xi + 1) * (eta - 1) / -4,
                  (xi + 1) * (eta + 1) / 4,
                  (xi - 1) * (eta + 1) / -4]).reshape(4, 1)
 
# These are the integration point data for four-point quadrature rule
xe = array([[-0.577350269189626, -0.577350269189626],
            [-0.577350269189626, +0.577350269189626],
            [+0.577350269189626, -0.577350269189626],
            [+0.577350269189626, +0.577350269189626]])
W = array([1, 1, 1, 1])

# These are the integration point data for one-point quadrature rule
xe = array([[+0., +0]])
W = array([4.0])

Kg = zeros((N_f, N_f))
Lg = zeros((N_f, 1))
 
nnpe = conn.shape[1] # number of nodes per element
zconn = conn - 1
for index  in arange(conn.shape[0]):
    # Element degree-of-freedom array,  converted to zero base
    zedof = dof[zconn[index, :]]-1
    # Initialize the elementwise conductivity matrix.
    Ke = zeros((nnpe, nnpe))
    # Initialize the elementwise heat load vector   
    LQe = zeros((nnpe, 1))
    # Loop over the quadrature points.
    for qp in range(xe.shape[0]):
        xi, eta = xe[qp, 0], xe[qp, 1]
        lx = x[zconn[index, :], :]
        # Compute the Jacobian matrix
        J = dot(lx.T, gradNpar(xi, eta))
        # The Jacobian
        detJ = linalg.det(J)
        # The  gradient  of the basis functions with respect to x,y
        gradN = dot(gradNpar(xi, eta), linalg.inv(J))
        # Add the contribution to the conductivity matrix
        Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]
        # At the contribution to the elementwise heat load vector
        LQe = LQe + Q * Dz * N(xi, eta) * ones((nnpe, 1)) * detJ * W[qp]
    # Assemble elementwise conductivity matrix
    for ro  in range(len(zedof)):
        for co  in range(len(zedof)):
            if (zedof[ro] < N_f) and (zedof[co] < N_f):
                Kg[zedof[ro], zedof[co]] = Kg[zedof[ro], zedof[co]] + Ke[ro, co]
    # Assemble the elementwise heat load vector   
    for ro  in range(len(zedof)):
        if (zedof[ro] < N_f):
            Lg[zedof[ro]] = Lg[zedof[ro]] + LQe[ro]   

# Solve for the global temperatures at the free degrees of freedom
Tg = linalg.solve(Kg, Lg)
print('Tg=', Tg)

# Set up an array of the temperatures at all the degrees of freedom
T = zeros((nnodes, 1))
T[0:N_f] = Tg  # insert the computed values of the free degrees of freedom

#for index  in arange(len(Tg)):
#    print(Tg[index])
#    
#for index  in arange(len(Lg)):
#    print(Lg[index])
#for index  in arange(len(Lg)):
#    print(Kg[index, :])
## Plot filled contours
#fig = plt.figure()
#ax = fig.add_subplot(111, projection='3d')
## setup three 1-d arrays for the x-coordinate, the y-coordinate, and the
## z-coordinate
#xs = x[:, 0].reshape(nnodes,)  # one value per node
#ys = x[:, 1].reshape(nnodes,)  # one value per node
#ix = dof[range(nnodes)] - 1
#zs = (T[ix]).reshape(nnodes,)  # one value per node
#triangles = conn[:, (0, 1, 3)] - 1  # the triangles are defined by the connectivity arrays
#ax.plot_trisurf(xs, ys, zs, cmap = cm.jet, linewidth=0)
##ax.plot_trisurf(xs, ys, triangles, zs, cmap = cm.jet)
#triangles = conn[:, (1, 2, 3)] - 1  # the triangles are defined by the connectivity arrays
#ax.plot_trisurf(xs, ys, zs, cmap = cm.jet, linewidth=0)
##ax.plot_trisurf(xs, ys, triangles, zs, cmap = cm.jet)
##plt.colorbar()
#plt.title('Surface plot of temperature')
#plt.xlabel('x (m)')
#plt.ylabel('y (m)')
#plt.show()

# Plot filled contours
plt.figure()
plt.gca().set_aspect('equal')
# setup three 1-d arrays for the x-coordinate, the y-coordinate, and the
# z-coordinate
xs = x[:, 0].reshape(nnodes,)  # one value per node
ys = x[:, 1].reshape(nnodes,)  # one value per node
ix = dof[arange(nnodes)] - 1
zs = (T[ix]).reshape(nnodes,)  # one value per node
triangles = conn[:, (0, 1, 2)] - 1  # the triangles are defined by the connectivity arrays
plt.tricontour(xs, ys, triangles, zs, linewidths=3)
triangles = conn[:, (0, 2, 3)] - 1  # the triangles are defined by the connectivity arrays
plt.tricontour(xs, ys, triangles, zs, linewidths=3)
plt.colorbar()
plt.title('Contour plot of temperature')
plt.xlabel('x (m)')
plt.ylabel('y (m)')
plt.show()
