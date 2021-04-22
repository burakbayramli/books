# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Rotation of the stresses.
 """

import math
from numpy import array, argsort, cross
from numpy import linalg

sig = array([[-350.00, -150.00, 0],
             [-150.0, -350.00, 0],
             [0, 0, -200.0000]])

exb = array([math.sqrt(2.)/2, math.sqrt(2.)/2, 0])
  
ezb = array([0, 0, 1.])

eyb = cross(ezb, exb)

# The vectors exb, eyb, ezb are going to become the rows
# of RmT; to get the actual rotation matrix we need
# to transpose
Rm = array([exb, eyb, ezb]).T

sigb = dot(Rm.T, sig).dot(Rm)
