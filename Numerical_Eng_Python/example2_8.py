#!/usr/bin/python
## example2_8
from numarray import array,matrixmultiply,transpose
from choleski import *

a = array([[ 1.44, -0.36,  5.52,  0.0], \
           [-0.36, 10.33, -7.78,  0.0], \
           [ 5.52, -7.78, 28.40,  9.0], \
           [ 0.0,   0.0,   9.0,  61.0]])
L = choleski(a)
print 'L =\n',L
print '\nCheck: L*L_transpose =\n', \
      matrixmultiply(L,transpose(L))
raw_input("\nPress return to exit")  
