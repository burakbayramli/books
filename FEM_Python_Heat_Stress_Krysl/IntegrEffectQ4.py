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
from numpy import array, zeros, ones, arange, linspace
from numpy import unique, dot, linalg
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

import time

tstart = time.clock() 

# Input parameters
kappa = 0.2 # thermal conductivity
Q = 0.005 # uniform heat source
sidel = 48 # length of the side of the square domain
nes = 18 # number of elements per side
Dz = 1.0  # thickness of the slice

# Generate the nodes and the connectivities of the quadrilaterals
xs = linspace(0, sidel, nes+1) # grid in the X direction
ys = linspace(0, sidel, nes+1) # grid in the Y direction
nx = len(xs) - 1 # number of element edges in the X direction
ny = len(ys) - 1 # number of element edges in the Y direction
nnodes = (nx+1) * (ny+1) # Number of nodes
nfes = nx * ny # Number of elements

# Generate the nodes
x = zeros((nnodes, 2)) # Coordinates of nodes: initialized to all zeros
k = 0
for j  in arange(0, (ny+1)):
    for i  in arange(0, (nx+1)):
        x[k, :] = [xs[i], ys[j]]
        k = k + 1

# Generate the elements: note that the nodes are numbered from 1
conn = zeros((nfes, 4), dtype=int)
k = 0
for i  in arange(0, (nx)):
    for j  in arange(0, (ny)):
        f = int(j * (nx+1) + i) + 1
        conn[k, :] = [f, f+1, f+(nx+1)+1, f+(nx+1)]
        k = k + 1

# Define the degrees of freedom
# Fixed temperature at the nodes on the boundary, except for a few
# in the lower-left corner.  The  node numbers are one-based
Fixed = [int(i)+1 for i in arange(nx/2, nx+1)]
Fixed = Fixed + [int(j*(nx+1))+1 for j in arange(ny/2, ny+1)]
Fixed = Fixed + [int(i+ny*(nx+1))+1 for i in arange(0, nx+1)]
Fixed = Fixed + [int(j*(nx+1)+ny)+1 for j in arange(0, ny+1)]
Fixed = unique(Fixed)

dof = zeros((nnodes,1), dtype=int)
N_f = nnodes-len(Fixed) 
# Number the degrees of freedom: First we will number the prescribed 
# degrees of freedom starting from the number of free degrees of freedom
k = N_f # first we number the prescribed degrees of freedom
for index  in Fixed:
    dof[index-1] = k+1 # the DOF are 1-based
    k = k + 1
k = 0 # next we number the free degrees of freedom  
for index  in range(0, nnodes):
    if (dof[index] == 0): 
        dof[index] = k+1 # the DOF are 1-based
        k = k + 1

    
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
#xe = array([[+0., +0]])
#W = array([4.0])

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

tend = time.clock() 
print( 'Time =', tend-tstart)

print('Tg=', Tg)

# Set up an array of the temperatures at all the degrees of freedom
T = zeros((nnodes, 1))
T[0:N_f] = Tg  # insert the computed values of the free degrees of freedom

# Plot contours
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
