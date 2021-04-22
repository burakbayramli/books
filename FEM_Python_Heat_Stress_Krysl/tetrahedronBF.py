# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Computation of tetrahedron basis functions 
from the interpolation conditions.
 """

import math
from numpy import array
from numpy import linalg

X = array([[1.00788e+000,  1.38768e+000,  1.00000e+000,     1.0],
       [1.76777e+000,  1.76777e+000,  1.00000e+000,     1.0],
       [9.56709e-001,  2.30970e+000,  0.00000e+000,     1.0],
       [9.56709e-001,  2.30970e+000,  1.00000e+000,     1.0]])
A = linalg.inv(X)
print(A)

gradN = A[0:3, :].T
print(gradN)     

gradT = 2.07121*gradN[0, :]
print(gradT)

print(linalg.norm(-1.8*gradT))