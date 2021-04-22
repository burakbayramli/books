# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Computation of the principal stresses.
 """

import math
from numpy import array, argsort
from numpy import linalg

sig = array([[-350.00, -150.00, 0],
      [-150.0, -350.00, 0],
         [0, 0, -200.0000]])
D, V = linalg.eig(sig)
print(D)
print(V)

order = argsort(D)
print('Sorted principal stresses')
print(D[order[::-1]])
print('Sorted principal directions')
print(V[:, order[::-1]])