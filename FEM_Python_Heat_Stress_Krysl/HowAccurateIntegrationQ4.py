# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
A single four-node quadrilateral. Explore how accurate different quadrature rules are.
 """

import math
from numpy import array, zeros, ones, arange, linspace
from numpy import unique, dot, linalg
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

import time

# Input parameters
kappa = 0.2 # thermal conductivity
Dz = 1.0  # thickness of the slice

# The node locations
x = array([[-1.0, -1.0], [2.0, 0.0], [1.0, 3.0], [0.0, 2.0]])

# Generate the elements: note that the nodes are numbered from 1
conn = array([[1, 2, 3, 4]]).reshape((1, 4))

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

nnpe = conn.shape[1] # number of nodes per element
zconn = conn - 1

# Gauss one-point quadrature rule
xe = array([[+0., +0]])
W = array([4.0])

# Initialize the elementwise conductivity matrix.
Ke = zeros((nnpe, nnpe))
# Loop over the quadrature points.
for qp in range(xe.shape[0]):
    xi, eta = xe[qp, 0], xe[qp, 1]
    lx = x[zconn[0, :], :]
    # Compute the Jacobian matrix
    J = dot(lx.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]

Ke1 = Ke.copy()

# Gauss rule with 2 x 2 points
# These are the integration point data for four-point quadrature rule
xe = array([[-0.577350269189626, -0.577350269189626],
            [-0.577350269189626, +0.577350269189626],
            [+0.577350269189626, -0.577350269189626],
            [+0.577350269189626, +0.577350269189626]])
W = array([1, 1, 1, 1])

# Initialize the elementwise conductivity matrix.
Ke = zeros((nnpe, nnpe))
# Loop over the quadrature points.
for qp in range(xe.shape[0]):
    xi, eta = xe[qp, 0], xe[qp, 1]
    lx = x[zconn[0, :], :]
    # Compute the Jacobian matrix
    J = dot(lx.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]

Ke2 = Ke.copy()

# Gauss rule with 3 x 3 points
xe = array([[-0.774596669241483, -0.774596669241483],
            [-0.774596669241483, 0],
            [-0.774596669241483, 0.774596669241483],
            [0, -0.774596669241483],
            [0, 0],
            [0, 0.774596669241483],
            [0.774596669241483, -0.774596669241483],
            [0.774596669241483, 0],
            [0.774596669241483, 0.774596669241483]])
W = array([ 0.308641975308642, 0.493827160493827, 0.308641975308642, 0.493827160493827, 0.790123456790124, 0.493827160493827, 0.308641975308642, 0.493827160493827, 0.308641975308642])


# Initialize the elementwise conductivity matrix.
Ke = zeros((nnpe, nnpe))
# Loop over the quadrature points.
for qp in range(xe.shape[0]):
    xi, eta = xe[qp, 0], xe[qp, 1]
    lx = x[zconn[0, :], :]
    # Compute the Jacobian matrix
    J = dot(lx.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]

Ke3 = Ke.copy()

print('Ke1 = ', Ke1)
print()
print('Ke2 = ', Ke2)
print()
print('Ke3 = ', Ke3)
print()


# Gauss rule with 4 x 4 points
xe = array([[-0.86113631159405,-0.86113631159405],
    [-0.86113631159405,-0.33998104358486],
    [-0.86113631159405,0.33998104358486],
    [-0.86113631159405,0.86113631159405],
    [-0.33998104358486,-0.86113631159405],
    [-0.33998104358486,-0.33998104358486],
    [-0.33998104358486,0.33998104358486],
    [-0.33998104358486,0.86113631159405],
    [0.33998104358486,-0.86113631159405],
    [0.33998104358486,-0.33998104358486],
    [0.33998104358486,0.33998104358486],
    [0.33998104358486,0.86113631159405],
    [0.86113631159405,-0.86113631159405],
    [0.86113631159405,-0.33998104358486],
    [0.86113631159405,0.33998104358486],
    [0.86113631159405,0.86113631159405]
    ])
W = array([[0.121002993285599],
    [0.226851851851851],
    [0.226851851851851],
    [0.121002993285599],
    [0.226851851851851],
    [0.425293303010699],
    [0.425293303010699],
    [0.226851851851851],
    [0.226851851851851],
    [0.425293303010699],
    [0.425293303010699],
    [0.226851851851851],
    [0.121002993285599],
    [0.226851851851851],
    [0.226851851851851],
    [0.121002993285599]
    ])

# Initialize the elementwise conductivity matrix.
Ke = zeros((nnpe, nnpe))
# Loop over the quadrature points.
for qp in range(xe.shape[0]):
    xi, eta = xe[qp, 0], xe[qp, 1]
    lx = x[zconn[0, :], :]
    # Compute the Jacobian matrix
    J = dot(lx.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]

Ke4 = Ke.copy()

print('Ke4 = ', Ke4)
print()

# Gauss rule with 5 x 5 points
xe = array([[-0.906179845938664,-0.906179845938664],
    [-0.906179845938664,-0.538469310105683],
    [-0.906179845938664,4.75059282543674e-17],
    [-0.906179845938664,0.538469310105683],
    [-0.906179845938664,0.906179845938664],
    [-0.538469310105683,-0.906179845938664],
    [-0.538469310105683,-0.538469310105683],
    [-0.538469310105683,4.75059282543674e-17],
    [-0.538469310105683,0.538469310105683],
    [-0.538469310105683,0.906179845938664],
    [4.75059282543674e-17,-0.906179845938664],
    [4.75059282543674e-17,-0.538469310105683],
    [4.75059282543674e-17,4.75059282543674e-17],
    [4.75059282543674e-17,0.538469310105683],
    [4.75059282543674e-17,0.906179845938664],
    [0.538469310105683,-0.906179845938664],
    [0.538469310105683,-0.538469310105683],
    [0.538469310105683,4.75059282543674e-17],
    [0.538469310105683,0.538469310105683],
    [0.538469310105683,0.906179845938664],
    [0.906179845938664,-0.906179845938664],
    [0.906179845938664,-0.538469310105683],
    [0.906179845938664,4.75059282543674e-17],
    [0.906179845938664,0.538469310105683],
    [0.906179845938664,0.906179845938664]
    ])
W = array([[0.0561343488624286],
    [0.1134],
    [0.134785072387521],
    [0.1134],
    [0.0561343488624286],
    [0.1134],
    [0.229085404223991],
    [0.272286532550751],
    [0.229085404223991],
    [0.1134],
    [0.134785072387521],
    [0.272286532550751],
    [0.323634567901234],
    [0.272286532550751],
    [0.134785072387521],
    [0.1134],
    [0.229085404223991],
    [0.272286532550751],
    [0.229085404223991],
    [0.1134],
    [0.0561343488624286],
    [0.1134],
    [0.134785072387521],
    [0.1134],
    [0.0561343488624285]
    ])

# Initialize the elementwise conductivity matrix.
Ke = zeros((nnpe, nnpe))
# Loop over the quadrature points.
for qp in range(xe.shape[0]):
    xi, eta = xe[qp, 0], xe[qp, 1]
    lx = x[zconn[0, :], :]
    # Compute the Jacobian matrix
    J = dot(lx.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]

Ke5 = Ke.copy()

print('Ke5 = ', Ke5)
print()


# Gauss rule with 6 x 6 points
xe = array([[-0.932469514203152,-0.932469514203152],
    [-0.932469514203152,-0.661209386466264],
    [-0.932469514203152,-0.238619186083197],
    [-0.932469514203152,0.238619186083197],
    [-0.932469514203152,0.661209386466264],
    [-0.932469514203152,0.932469514203152],
    [-0.661209386466264,-0.932469514203152],
    [-0.661209386466264,-0.661209386466264],
    [-0.661209386466264,-0.238619186083197],
    [-0.661209386466264,0.238619186083197],
    [-0.661209386466264,0.661209386466264],
    [-0.661209386466264,0.932469514203152],
    [-0.238619186083197,-0.932469514203152],
    [-0.238619186083197,-0.661209386466264],
    [-0.238619186083197,-0.238619186083197],
    [-0.238619186083197,0.238619186083197],
    [-0.238619186083197,0.661209386466264],
    [-0.238619186083197,0.932469514203152],
    [0.238619186083197,-0.932469514203152],
    [0.238619186083197,-0.661209386466264],
    [0.238619186083197,-0.238619186083197],
    [0.238619186083197,0.238619186083197],
    [0.238619186083197,0.661209386466264],
    [0.238619186083197,0.932469514203152],
    [0.661209386466264,-0.932469514203152],
    [0.661209386466264,-0.661209386466264],
    [0.661209386466264,-0.238619186083197],
    [0.661209386466264,0.238619186083197],
    [0.661209386466264,0.661209386466264],
    [0.661209386466264,0.932469514203152],
    [0.932469514203152,-0.932469514203152],
    [0.932469514203152,-0.661209386466264],
    [0.932469514203152,-0.238619186083197],
    [0.932469514203152,0.238619186083197],
    [0.932469514203152,0.661209386466264],
    [0.932469514203152,0.932469514203152]
    ])
W = array([[0.0293520816889805],
    [0.0618072933723834],
    [0.0801651173178067],
    [0.0801651173178069],
    [0.0618072933723834],
    [0.0293520816889805],
    [0.0618072933723834],
    [0.130148912588167],
    [0.168805367087588],
    [0.168805367087588],
    [0.130148912588167],
    [0.0618072933723834],
    [0.0801651173178067],
    [0.168805367087588],
    [0.218943450167296],
    [0.218943450167297],
    [0.168805367087588],
    [0.0801651173178066],
    [0.0801651173178069],
    [0.168805367087588],
    [0.218943450167297],
    [0.218943450167298],
    [0.168805367087588],
    [0.0801651173178068],
    [0.0618072933723834],
    [0.130148912588167],
    [0.168805367087588],
    [0.168805367087588],
    [0.130148912588167],
    [0.0618072933723834],
    [0.0293520816889805],
    [0.0618072933723834],
    [0.0801651173178066],
    [0.0801651173178068],
    [0.0618072933723834],
    [0.0293520816889804]
    ])


# Initialize the elementwise conductivity matrix.
Ke = zeros((nnpe, nnpe))
# Loop over the quadrature points.
for qp in range(xe.shape[0]):
    xi, eta = xe[qp, 0], xe[qp, 1]
    lx = x[zconn[0, :], :]
    # Compute the Jacobian matrix
    J = dot(lx.T, gradNpar(xi, eta))
    # The Jacobian
    detJ = linalg.det(J)
    # The  gradient  of the basis functions with respect to x,y
    gradN = dot(gradNpar(xi, eta), linalg.inv(J))
    # Add the contribution to the conductivity matrix
    Ke = Ke + kappa * Dz * dot(gradN, gradN.T) * detJ * W[qp]

Ke6 = Ke.copy()

print('Ke6 = ', Ke6)
print()