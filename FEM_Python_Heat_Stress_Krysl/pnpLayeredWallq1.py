# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
 Layered wall.  Temperature boundary condition on the left, and heat flux
 boundary condition on the right.
 """

from numpy import array as array
from numpy import zeros as zeros
from numpy import arange as arange
from numpy import dot as dot
from numpy import linalg as linalg
from numpy import vstack  as vstack
from numpy import diff as diff
import matplotlib.pyplot as plt


Dz = 1.0 # Thickness of the slice (it cancels out  in the end)
kappaL = 0.05  # thermal conductivity, layer on the left
kappaR = 1.8  # thermal conductivity, layer on the right
# Coordinates of nodes
x = array([[0, 0], [0.07, 0], [0.3, 0], [0, 0.1], [0.07, 0.1], [0.3, 0.1]])
N = 6 # total number of nodes
N_f = 4 # total number of free degrees of freedom
# Mapping from nodes to  degrees of freedom
node2dof = array([5, 1, 3, 6, 2, 4])

# Connectivity of the left-region triangles (polyurethane)
connL = array([[4, 1, 5], [2, 5, 1]])
# Connectivity of the right-region triangles (concrete)
connR = array([[6, 5, 2], [2, 3, 6]])
# Connectivity  of the boundary L2 element  on the right
connRbdry = array([[3, 6]])
pTe = -10  # Boundary conditions, exterior, interior, deg Celsius
Tfix = zeros(N).reshape(N, 1)
for index  in [1, 4]:
    Tfix[node2dof[index-1]-1] = pTe # left face with EBC, nodes 1 and 4
qnbarR = -30

# gradients of the basis functions with respect to the param. coordinates
gradNpar = array([[-1, -1], [1, 0], [0, 1]])

Kg = zeros((N_f, N_f)) # allocate the global conductivity matrix
Lg = zeros(N_f).reshape(N_f, 1)  # allocate the global heat loads vector

# Loop over the triangles in the mesh, left domain
kappa = kappaL
zconn = array(connL)-1
for j  in arange(zconn.shape[0]):
    J = dot(x[zconn[j, :], :].T, gradNpar) # compute the Jacobian matrix
    gradN = dot(gradNpar, linalg.inv(J)) # compute the x,y grads of the b. funcs
    Ke = (kappa*Dz*linalg.det(J)/2)*dot(gradN, gradN.T) # elementwise matrix
    # Element degree-of-freedom array,  converted to zero base
    zedof = array(node2dof[zconn[j, :]])-1
    # Assemble elementwise conductivity matrix
    for ro  in arange(len(zedof)):
        for co  in arange(len(zedof)):
            if (zedof[ro] < N_f) and (zedof[co] < N_f):
                Kg[zedof[ro], zedof[co]] = Kg[zedof[ro], zedof[co]] + Ke[ro, co]
    # Compute the elementwise load vector due to prescribed temperatures
    LTe = zeros(3).reshape(3, 1)
    for co  in arange(len(zedof)):
        if zedof[co] >= N_f:
            LTe= LTe - Ke[:, co].reshape(3, 1)*Tfix[zedof[co]] # the "-" sign!
    # Assemble elementwise heat load vector
    for ro  in arange(len(zedof)):
        if zedof[ro] < N_f:
            Lg[zedof[ro]] = Lg[zedof[ro]] + LTe[ro]

# Loop over the triangles in the mesh, right domain
kappa = kappaR
zconn = array(connR)-1
for j  in arange(zconn.shape[0]):
    J = dot(x[zconn[j, :], :].T, gradNpar) # compute the Jacobian matrix
    gradN = dot(gradNpar, linalg.inv(J)) # compute the x,y grads of the b. funcs
    Ke = (kappa*Dz*linalg.det(J)/2)*dot(gradN, gradN.T) # elementwise matrix
    # Element degree-of-freedom array,  converted to zero base
    zedof = array(node2dof[zconn[j, :]])-1
    # Assemble elementwise conductivity matrix
    for ro  in arange(len(zedof)):
        for co  in arange(len(zedof)):
            if (zedof[ro] < N_f) and (zedof[co] < N_f):
                Kg[zedof[ro], zedof[co]] = Kg[zedof[ro], zedof[co]] + Ke[ro, co]
    # Compute the elementwise load vector due to prescribed temperatures
    LTe = zeros(3).reshape(3, 1)
    for co  in arange(len(zedof)):
        if zedof[co] >= N_f:
            LTe= LTe - Ke[:, co].reshape(3, 1)*Tfix[zedof[co]] # the "-" sign!
    # Assemble elementwise heat load vector
    for ro  in arange(len(zedof)):
        if zedof[ro] < N_f:
            Lg[zedof[ro]] = Lg[zedof[ro]] + LTe[ro]

# Boundary element heat flux term
qnbar = qnbarR
zconn = connRbdry - 1
for j  in arange(zconn.shape[0]):
    he = linalg.norm(diff(x[zconn[j, :], :], axis=0))
    # Element degree-of-freedom array,  converted to zero base
    zedof = array(node2dof[zconn[j, :]])-1
    Leq = -qnbar*he*Dz/2*array([[1], [1]])
    # Assemble elementwise heat load vector
    for ro  in arange(len(zedof)):
        if (zedof[ro] < N_f):
            Lg[zedof[ro]] = Lg[zedof[ro]] + Leq[ro]

# Solve for the global temperatures at the free degrees of freedom
Tg = linalg.solve(Kg, Lg)
print('Tg=', Tg)

# Set up an array of the temperatures at all the degrees of freedom
T = Tfix.copy()
T[0:N_f] = Tg # insert the computed values of the free degrees of freedom

# Plotting
plt.figure()
plt.gca().set_aspect('equal')
# setup three 1-d arrays for the x-coord, the y-coord, and the z-coordinate
xs = x[:, 0].reshape(N,)# one value per node
ys = x[:, 1].reshape(N,)# one value per node
ix = node2dof[arange(N)]-1
zs = (T[ix]).reshape(N,)# one value per node
triangles = vstack((connL-1, connR-1))# triangles are defined by the conn arrays
plt.tricontourf(xs, ys, triangles, zs)
plt.colorbar()
plt.title('Contour plot of temperature')
plt.xlabel('x (m)')
plt.ylabel('y (m)')
plt.show()
