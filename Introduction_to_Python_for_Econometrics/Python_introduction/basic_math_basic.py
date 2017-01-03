from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = 9
y = 5
(type(x), type(y))
x/y # Since division imported
float(x)/y

x = array([[1,2,3.0]])
x
y = array([[0],[0],[0.0]])
y
x + y  # Adding 0 produces broadcast

x = reshape(arange(15),(3,5))
x
y = 1
x + y - x
y = arange(5)
y
x + y - x
y = arange(3)
y
try:
    x + y - x # Error
except:
    print("Error detected in: x + y - x # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

x = asmatrix(randn(2,2))
xpx1 = x.T * x
xpx2 = x.transpose() * x
xpx3 = transpose(x) * x

