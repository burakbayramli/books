# Finite Element Modeling with Abaqus and Python for  Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Concrete column with hydration heat and convection (film) condition
on the boundary. Numerical solution.
"""

from numpy import array as array
from numpy import zeros as zeros
from numpy import arange as arange
from numpy import dot as dot
from numpy import linalg as linalg
from numpy import vstack  as vstack
from numpy import diff as diff
import matplotlib.pyplot as plt

# These are the input constants in the problem:
a = 2.5  # radius of the column cross-section
dy = a / 2 * math.sin(15. / 180 * math.pi)
dx = a / 2 * math.cos(15. / 180 * math.pi)
Q = 4.5  # internal heat generation rate
k = 1.8  # thermal conductivity
Dz = 1.0  # thickness of the slice
h = 5.0  # surface heat transfer coefficient
Ta = 0.0  # ambient temperature (freezing water)

# Coordinates of nodes
x = array([[0, 0], [dx, -dy], [dx, dy], [2 * dx, -2 * dy], [2 * dx, 2 * dy]])
N = 5  # total number of nodes
N_f = 5  # total number of free degrees of freedom
# Mapping from nodes to  degrees of freedom
node2dof = array([3, 1, 2, 5, 4])

# gradients of the basis functions with respect to the param. coordinates
gradNpar = array([[-1, -1], [1, 0], [0, 1]])

# Connectivity of the mesh: interior mesh
conn = array([[1, 2, 3], [2, 4, 5], [2, 5, 3]])
# boundary mesh
connbdry = array([[4, 5]])

Kg = zeros((N_f, N_f))  # allocate the global conductivity matrix
Lg = zeros((N_f, 1))  # allocate the global heat loads vector

# Loop over the triangles in the mesh
kappa = k
zconn = conn - 1
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
    LQe = linalg.det(J)/2*Q*Dz/3*ones((3, 1))
    for ro  in arange(len(zedof)):
        Lg[zedof[ro]] = Lg[zedof[ro]] + LQe[ro]

# Calculations for the  convection boundary condition
Hg = zeros((N_f, N_f))  # allocate the global film-condition matrix
zconn = connbdry - 1
for j in arange(zconn.shape[0]):
    he = linalg.norm(diff(x[zconn[j, :], :], axis=0))
    # Element degree-of-freedom array,  converted to zero base
    zedof = array(node2dof[zconn[j, :]]) - 1
    He = h * Dz * he / 6 * array([[2, 1], [1, 2]])
    # Assemble elementwise surface heat transfer matrix
    for ro in arange(len(zedof)):
        for co in arange(len(zedof)):
            if (zedof[ro] < N_f) and (zedof[co] < N_f):
                Hg[zedof[ro], zedof[co]] = Hg[zedof[ro], zedof[co]] + He[ro, co]
    # Assemble elementwise heat load vector for surface heat transfer
    Lea = Ta * h * he * Dz / 2 * array([[1], [1]])
    for ro in arange(len(zedof)):
        if (zedof[ro] < N_f):
            Lg[zedof[ro]] = Lg[zedof[ro]] + Lea[ro]

# Solve for the global temperatures at the free degrees of freedom
Tg = linalg.solve((Kg + Hg), Lg)
print('Tg=', Tg)

print('Temperatures at the nodes')
for index in arange(node2dof.shape[0]):
    print('Node', index + 1, 'T=', Tg[node2dof[index] - 1])

# Set up an array of the temperatures at all the degrees of freedom
T = Tg.copy()
T[0:N_f] = Tg  # insert the computed values of the free degrees of freedom

# Plot filled contours
plt.figure()
plt.gca().set_aspect('equal')
# setup three 1-d arrays for the x-coordinate, the y-coordinate, and the
# z-coordinate
xs = x[:, 0].reshape(N,)  # one value per node
ys = x[:, 1].reshape(N,)  # one value per node
ix = node2dof[arange(N)] - 1
zs = (T[ix]).reshape(N,)  # one value per node
triangles = conn - 1  # the triangles are defined by the connectivity arrays
plt.tricontourf(xs, ys, triangles, zs)
plt.colorbar()
plt.title('Contour plot of temperature')
plt.xlabel('x (m)')
plt.ylabel('y (m)')
plt.show()