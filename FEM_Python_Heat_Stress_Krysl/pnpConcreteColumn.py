# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
"""
Concrete column with hydration heat and zero temperature on the boundary.
Numerical solution. This process is shown step-by-step, which means that 
the listing is much longer than it needs to be. Later we use loops, which 
makes the code much more succinct.
"""

import math
from numpy import array as array
from numpy import zeros as zeros
from numpy import ones as ones
from numpy import arange as arange
from numpy import dot as dot
from numpy import linalg as linalg

# These are the constants in the problem, k is kappa
a = 2.5 # radius on the columnthe
dy = a/2*math.sin(15./180*math.pi)
dx = a/2*math.cos(15./180*math.pi)
Q = 4.5 # internal heat generation rate
k = 1.8 # thermal conductivity
Dz = 1.0 # thickness of the slice

#  Gradients of the basis functions wrt the parametric coords
gradNpar = array([[-1, -1], [1, 0], [0, 1]])
#Coordinates of the nodes. Node 1 in first row, and so on.
xall = array([[0, 0], [dx, -dy], [dx, dy], [2*dx, -2*dy], [2*dx, 2*dy]])
# Numbers of the degrees of freedom
dof = array([3, 1, 2, 5, 4])
# Number of free degrees of freedom
N_f = 3

# Global conductivity matrix and heat load vector.
K = zeros((N_f, N_f))
L = zeros((N_f,)).reshape(N_f, 1)

#First element
conn = array([1, 2, 3]) # The definition of the element, listing its nodes
zconn = conn - 1 # zero-based node indexes
x = xall[zconn, :]# The coordinates  of the three nodes
print('x = ', x)
J = dot(x.T, gradNpar) # Compute the Jacobian matrix
Se = linalg.det(J)/2 # The area of the triangle
print('Se = ', Se)
# Compute the gradient with respect to X, Y
gradN = dot(gradNpar, linalg.inv(J))
print('gradN = ', gradN)
# Some terms of the conductivity matrix
print(Se*dot(gradN[0, :], gradN[0, :].T)*k*Dz)
print(Se*dot(gradN[0, :], gradN[1, :].T)*k*Dz)
# The entire elementwise conductivity matrix
Ke1 = (Se*dot(gradN, gradN.T)*k*Dz)
print('Ke1 = ', Ke1)
# Element degree-of-freedom array,  converted to zero base
zedof = array(dof[zconn])-1
# Assemble contribution from element 1
for ro  in arange(len(zedof)):
    for co  in arange(len(zedof)):
        K[zedof[ro], zedof[co]] = K[zedof[ro], zedof[co]] + Ke1[ro, co]

print('K = ', K)
# Compute heat load from element 1
LQe1 = Se*Q*Dz/3*ones((3,)).reshape(3, 1)
for ro  in arange(len(zedof)):
    L[zedof[ro]] = L[zedof[ro]] + LQe1[ro]

##Second element
conn = array([2, 4, 5])
zconn = conn - 1 # zero-based node indexes
x = xall[zconn, :]# The coordinates  of the three nodes
J = dot(x.T, gradNpar) # Compute the Jacobian matrix
Se = linalg.det(J)/2 # The area of the triangle
# Compute the gradient with respect to X, Y
gradN = dot(gradNpar, linalg.inv(J))
# The entire elementwise conductivity matrix
Ke2 = (Se*dot(gradN, gradN.T)*k*Dz)
print(Ke2)
# Element degree-of-freedom array,  converted to zero base
zedof = array(dof[zconn])-1
# Assemble contribution from element 2
for ro  in arange(len(zedof)):
    for co  in arange(len(zedof)):
        if (zedof[ro] < N_f) and (zedof[co] < N_f):
            K[zedof[ro], zedof[co]] = K[zedof[ro], zedof[co]] + Ke2[ro, co]

print(K)
# Compute heat load from element 1
LQe2 = Se*Q*Dz/3*ones((3,)).reshape(3, 1)
for ro  in arange(len(zedof)):
    if (zedof[ro] < N_f):
        L[zedof[ro]] = L[zedof[ro]] + LQe2[ro]

##Third element
conn = array([2, 5, 3])
zconn = conn - 1 # zero-based node indexes
x = xall[zconn, :]# The coordinates  of the three nodes
J = dot(x.T, gradNpar) # Compute the Jacobian matrix
Se = linalg.det(J)/2 # The area of the triangle
# Compute the gradient with respect to X, Y
gradN = dot(gradNpar, linalg.inv(J))
# The entire elementwise conductivity matrix
Ke3 = (Se*dot(gradN, gradN.T)*k*Dz)
# Element degree-of-freedom array,  converted to zero base
zedof = array(dof[zconn])-1
# Assemble contribution from element 2
for ro  in arange(len(zedof)):
    for co  in arange(len(zedof)):
        if (zedof[ro] < N_f) and (zedof[co] < N_f):
            K[zedof[ro], zedof[co]] = K[zedof[ro], zedof[co]] + Ke3[ro, co]

print(K)
# Compute heat load from element 1
LQe3 = Se*Q*Dz/3*ones((3,)).reshape(3, 1)
for ro  in arange(len(zedof)):
    if (zedof[ro] < N_f):
        L[zedof[ro]] = L[zedof[ro]] + LQe3[ro]

# Solution:
T = linalg.solve(K, L)
print('Solution T = ', T)
