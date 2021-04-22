# Finite Element Modeling with Abaqus and Python for  Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Derivation of the tetrahedron mass matrix.
"""

from numpy import array, zeros, dot, linalg
from numpy import vstack  as vstack
from numpy import diff as diff


# gradients of the basis functions with respect to the param. coordinates
gradNpar = array([[-1, -1, -1], 
                  [1, 0, 0], 
                  [0, 1, 0],
                  [0, 0, 1]])

# Array of vertex locations
x = array([[1.4727, 0.9193, -0.7713],
           [1.4727, 1.2000, 0],
           [1.4727, 0.4543, 0.0530],
           [3.0000, 0.5063, 0.0290]])

J = dot(x.T, gradNpar)

detJ = linalg.det(J)