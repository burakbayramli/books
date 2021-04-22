# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Computation of the traction components from stresses.
 """

import math
from numpy import array, dot
from numpy import linalg

sig = array([[9.00, -2.00, 0.00],
             [-2.00, -3.00, 2.00],
             [0.00, 2.00, 3.00]])

n = array([2.0, 1.0, 2.0])
n = n/linalg.norm(n)

t = dot(sig, n)

tn = dot(t, n)

ts = t - tn * n

print(linalg.norm(ts))

Pn = array([[n[0], 0, 0, n[1], n[2], 0],
            [0, n[1], 0, n[0], 0, n[2]],
            [0, 0, n[2], 0, n[0], n[1]]])

sigv = array([9.00, -3.00, 3.00, -2.00, 0.00, 2.00])
sigv.shape = (6, 1)
n.shape = (3, 1)

t = dot(Pn, sigv)
tn = dot(t.T, n)


ts = t - tn * n

print(linalg.norm(ts))