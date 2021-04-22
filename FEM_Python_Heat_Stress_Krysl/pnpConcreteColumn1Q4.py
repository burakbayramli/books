# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
 Concrete column with temperature boundary condition.  Finite element mesh
 that consists of a single four-node quadrilateral.
 """

import math
from numpy import array as array
from numpy import zeros as zeros
from numpy import arange as arange
from numpy import dot as dot
from numpy import linalg as linalg

##
# Evaluate for a given data
a = 2.5  # radius of the column
b = a * math.sin(15 / 180 * math.pi)  # dimension
h = a * math.cos(15 / 180 * math.pi)  # dimension
Q = 4.5  # internal heat generation rate
k = 1.8  # thermal conductivity
Dz = 1.0  # thickness of the slice
x = array([[0, 0], [h, -b], [a, 0], [h, b]])  # Coordinates of nodes

##
# Define a little function to calculate the gradient of the basis functions
# in the parametric coordinates.


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

# The expressions actually come from  the following symbolic code:
#xi,eta = symbols('xi eta')
#N=Matrix([(xi-1)*(eta-1)/4, (xi+1)*(eta-1)/-4, (xi+1)*(eta+1)/4, (xi-1)*(eta+1)/-4])
# N=N.reshape(4,1)
# gradNpar=diff(N,'xi').row_join(diff(N,'eta'))
# print(gradNpar)

##
# These are the integration point data
xe = array([[-0.577350269189626, -0.577350269189626],
            [-0.577350269189626, +0.577350269189626],
            [+0.577350269189626, -0.577350269189626],
            [+0.577350269189626, +0.577350269189626]])
W = array([1, 1, 1, 1])

##
# Initialize the elementwise conductivity matrix.
Ke = zeros((4, 4))
##
# Loop over the quadrature points.
for qp in arange(xe.shape[0]):
    xi = xe[qp, 0]
    eta = xe[qp, 1]
    # Compute the Jacobian matrix
    J = dot(x.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + k * Dz * dot(gradN, gradN.T) * detJ * W[qp]

print(Ke)

##
# We will find it convenient to define a little function to evaluate the
# basis function values at a given quadrature point location.


def N(xi, eta):
    """
    A one-liner to calculate the matrix of the basis function values.
    """
    return array([(xi - 1) * (eta - 1) / 4,
                  (xi + 1) * (eta - 1) / -4,
                  (xi + 1) * (eta + 1) / 4,
                  (xi - 1) * (eta + 1) / -4]).reshape(4, 1)

##
# Initialize the elementwise heat-load vector .
Le = zeros((4, 1))
##
# Loop over the quadrature points.
for qp in arange(xe.shape[0]):
    xi = xe[qp, 0]
    eta = xe[qp, 1]
    # Compute the Jacobian matrix
    J = dot(x.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # Add the contribution to the heat load vector
    Le = Le + Q * Dz * N(xi, eta) * detJ * W[qp]

print(Le)

# Global system matrix and global load vector
K = Ke
L = Le

##
# The solution vector consists of all zeros, except in the first entry.
T = zeros((4, 1))
# Solve the global equations
T[0] = (1 / K[0, 0]) * L[0]
print('T_1 = ', T[0])

#
# Now we will postprocess to extract the heat flux vectors at the
# quadrature points.
##
# Loop over the quadrature points.
for qp in arange(xe.shape[0]):
    xi = xe[qp, 0]
    eta = xe[qp, 1]
    # Compute the location of the quadrature point
    qploc = dot(N(xi, eta).T, x)
    # Compute the Jacobian matrix
    J = dot(x.T, gradNpar(xi, eta))
    # The  gradient  of the basis functions with respect to x, y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Compute the gradient of temperature
    gradT = dot(T.T, gradN)
    # Heat flux vector
    q = -k * gradT.T
    print('Heat flux at', qploc, '  = ', q)


