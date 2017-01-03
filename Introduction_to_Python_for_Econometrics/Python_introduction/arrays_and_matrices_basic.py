from __future__ import division
from __future__ import print_function
from pylab import log2 # Will import log2 only
from scipy import log10 # Will not import the log2 from SciPy
import numpy 
import numpy as np
import pylab 
import pylab as pl
import scipy
import scipy as sp
import sys
from pylab import *
from numpy import *
# End Imports


x = [0.0, 1, 2, 3, 4]
y = array(x)
y
type(y)

y = array([[0.0, 1, 2, 3, 4], [5, 6, 7, 8, 9]])
y
shape(y)
y = array([[[1,2],[3,4]],[[5,6],[7,8]]])
y
shape(y)

x = [0, 1, 2, 3, 4] # Integers
y = array(x)
y.dtype
x = [0.0, 1, 2, 3, 4] # 0.0 is a float
y = array(x)
y.dtype
x = [0.0 + 1j, 1, 2, 3, 4] # (0.0 + 1j) is a complex
y = array(x)
y
y.dtype

x = [0, 1, 2, 3, 4] # Integers
y = array(x)
y.dtype
y = array(x, dtype='float64') # String dtype
y.dtype
y = array(x, dtype=float64) # NumPy type dtype
y.dtype

x = [0.0,1, 2, 3, 4] # 1 Float makes all float
y = array(x)
type(y)
y * y # Element-by-element
z = asmatrix(x)
type(z)
try:
    z * z # Error
except:
    print("Error detected in: z * z # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

x = array([1.0,2.0,3.0,4.0,5.0])
x[0]
x = array([[1.0,2,3],[4,5,6]])
x[1,2]
type(x[1,2])

x = array([1.0,2.0,3.0,4.0,5.0])
x[0] = -5
x

x = array([1.0,2.0,3.0,4.0,5.0])
y = x[:]
y = x[:2]
y = x[1::2]

y = array([[0.0, 1, 2, 3, 4],[5, 6, 7, 8, 9]])
y
y[:1,:] # Row 0, all columns
y[:,:1] # all rows, column 0
y[:1,0:3] # Row 0, columns 0 to 2
y[:1][:,0:3] # Same as previous
y[:,3:] # All rows, columns 3 and 4
y = array([[[1.0,2],[3,4]],[[5,6],[7,8]]])
y[:1,:,:] # Panel 0 of 3D y

x = array([[1.0,2],[3,4]])
x[:1,:] # Row 1, all columns, 2-dimensional
x[0,:] # Row 1, all columns, dimension reduced

x = array([[0.0, 1, 2, 3, 4],[5, 6, 7, 8, 9]])
x[:1,:] # Row 0, all columns, 2-dimensional
ndim(x[:1,:])
x[0,:] # Row 0, all column, dim reduction to 1-d array
ndim(x[0,:])
x[0,0] # Top left element, dim reduction to scalar (0-d array)
ndim(x[0,0])
x[:,0] # All rows, 1 column, dim reduction to 1-d array

x = array([[0.0]*3]*3)  # *3 repeats the list 3 times
x
x[0,:] = array([1.0, 2.0, 3.0])
x
x[::2,::2] =  array([[-99.0,-99],[-99,-99]]) # 2 by 2
x
x[1,1] = pi
x

x = [0, 1, 2, 3, 4] # Integers
y = array(x)
y.dtype
y[0] = 3.141592
y
x = [0.0,1, 2, 3, 4] # 1 Float makes all float
y = array(x)
y.dtype
y[0] = 3.141592
y

y = reshape(arange(25.0),(5,5))
y
y[0] # Same as y[0,:], first row
y.flat[0] # Scalar slice, flat is 1-dimensional
try:
    y[6] # Error
except:
    print("Error detected in: y[6] # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))
y.flat[6] # Element 6
y.flat[12:15]
y.flat[:] # All element slice

x = reshape(arange(4.0),(2,2))
x
s1 = x[0,:] # First row
s2 = x[:,0] # First column
s1[0] = -3.14 # Assign first element
s1
s2
x

x = reshape(arange(4.0),(2,2))
s1 = copy(x[0,:]) # Function copy
s2 = x[:,0].copy() # Method copy
s3 = array(x[0,:]) # Create a new array
s1[0] = -3.14
s1
s2
s3
x[0,0]

x = arange(5.0)
y = x[0] # Pure scalar selection
z = x[:1] # A pure slice
y = -3.14
y # y Changes
x # No propagation
z  # No changes to z either
z[0] = -2.79
y # No propagation since y used pure scalar selection
x # z is a view of x, so changes propagate

x = array([[0.0, 1.0],[2.0,3.0]])
y = x
print(id(x),id(y)) # Same
y = x + 1.0
y
print(id(x),id(y)) # Different
x # Unchanged
y = exp(x)
print(id(x),id(y)) # Also Different




array(object, dtype=None, copy=True, order=None, subok=False, ndmin=0)

array([[1.0,2.0],[3.0,4.0]])
array([[1.0,2.0],[3.0,4.0]], 'int32')

array(object=[[1.0,2.0],[3.0,4.0]])
array([[1.0,2.0],[3.0,4.0]], dtype=None, copy=True, order=None, subok=False, ndmin=0)

array(dtype='complex64', object = [[1.0,2.0],[3.0,4.0]], copy=True)

x = array([[1.0,2.0],[3.0,4.0]])
s = shape(x)
s

x = array([[1.0,2.0],[3.0,4.0]])
M,N = shape(x)
M
N

try:
    M,N,P = shape(x) # Error
except:
    print("Error detected in: M,N,P = shape(x) # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

x = randn(10,10,10)
shape(x)
try:
    M,N = shape(x) # Error
except:
    print("Error detected in: M,N = shape(x) # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

