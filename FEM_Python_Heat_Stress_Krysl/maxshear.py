# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Computation of the traction components from stresses.
 """

import math
from numpy import array, argsort
from numpy import linalg

sig = array([[9.00, -2.00, 0.00],
             [-2.00, -3.00, 2.00],
             [0.00, 2.00, 3.00]])

princstrs, princdirs = linalg.eig(sig)

order = argsort(princstrs)
print('Sorted principal stresses')
princstrs = princstrs[order[::-1]]
print(princstrs)
print('Sorted principal directions')
princdirs = princdirs[:, order[::-1]]
print(princdirs)
