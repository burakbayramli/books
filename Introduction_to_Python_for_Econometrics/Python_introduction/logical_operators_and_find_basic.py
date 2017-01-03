from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = array([[1,2],[-3,-4]])
x > 0
x == -3
y = array([1,-1])
x < y # y broadcast to be (2,2)
z = array([[1,1],[-1,-1]]) # Same as broadcast y
x < z

eps = np.finfo(np.float64).eps
eps
x = randn(2)
y = x + eps
x == y
allclose(x,y)

x = randn(10,1)
y = tile(x,2)
array_equal(x,y)
array_equiv(x,y)

