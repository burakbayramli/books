from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = 10 * arange(5.0)
x[[0]] # List with 1 element
x[[0,2,1]] # List
sel = array([4,2,3,1,4,4]) # Array with repetition
x[sel]
sel = array([[4,2],[3,1]]) # 2 by 2 array
x[sel] # Selection has same size as sel
sel = array([0.0,1]) # Floating point data
try:
    x[sel] # Error
except:
    print("Error detected in: x[sel] # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))
x[sel.astype(int)] # No error
x[0] # Scalar selection, not numerical indexing

x = reshape(arange(10.0), (2,5))
x
sel = array([0,1])
x[sel,sel] # 1-dim arrays, no broadcasting
x[sel, sel+1]
sel_row = array([[0,0],[1,1]])
sel_col = array([[0,1],[0,1]])
x[sel_row,sel_col] # 2 by 2, no broadcasting
sel_row = array([[0],[1]])
sel_col = array([[0,1]])
x[sel_row,sel_col] # 2 by 1 and 1 by 2 - difference shapes, broadcasted as 2 by 2

sel_row = array([0,1]) # 1-dimensional with shape (2,)
sel_col = array([1,2,3]) # 1-dimensional with shape (3,)
try:
    x[sel_row,sel_col] # Error
except:
    print("Error detected in: x[sel_row,sel_col] # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

x[ix_([0,1],[1,2,3])]

sel=array([[1],[2]]) # 2 by 1
x[0,sel] # Row 0, elements sel
sel_row = array([[0],[0]])
x[sel_row,sel] # Identical

x[:,[1]]
x[[1],:]

x[:,1] # 1-dimensional

x = reshape(arange(3**3), (3,3,3)) # 3-d array
sel1 = x[::2,[1,0],:1]
sel2 = x[ix_(arange(0,3,2),[1,0],arange(0,1))]
sel1.shape
sel2.shape
amax(abs(sel1-sel2))

sel1 = x[[0,0],[1,0],:1]
step1 = x[:,:,:1]
step2 = x[[0,0],[1,0],:]
step2.shape
amax(abs(sel1-step2))

x = reshape(arange(4**4), (4,4,4,4))
sel = x[[0,1],[0,1],:2,:2] # 1-dimensional numerical and 2 slices
sel.shape

x.flat[[3,4,9]]
x.flat[[[3,4,9],[1,5,3]]]

x = arange(-3,3)
x < 0
x[x < 0]
x[abs(x) >= 2]
x = reshape(arange(-8, 8), (4,4))
x[x < 0]

x = reshape(arange(-8,8),(4,4))
cols = any(x < -6, 0)
rows = any(x < 0, 1)
cols
rows
x[cols,rows] # Not upper 2 by 2
x[ix_(cols,rows)] # Upper 2 by 2

cols.nonzero()
rows.nonzero()

cols = any(x < -6, 0)
rows = any(x < 4, 1)
rows
try:
    x[cols,rows] # Error
except:
    print("Error detected in: x[cols,rows] # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

x = reshape(arange(-8,8), (4,4))
x
sum(x, 0)
sum(x, 0) >= 0
x[0,sum(x, 0) >= 0]

sel = sum(x < -1, 0) >= 2
sel
x[:,sel] # All rows, sel selects columns
x[1:3,sel] # Rows 1 and 2, sel selects columns
x[sel,2:] # sel selects rows, columns 2 and 3

sel = array([True,True,False,False])
sel.nonzero()
x[[2,3],sel] # Elements (2,0) and (3,1)
x[[2,3],[0,1]] # Identical

x = randn(3)
x
argwhere(x<0.6)
argwhere(x<-10.0) # Empty array
x = randn(3,2)
x
argwhere(x<0)
argwhere(x<1)

x = randn(3)
x
extract(x<0, x)
extract(x<-10.0, x) # Empty array
x = randn(3,2)
x
extract(x>0,x)

x = reshape(arange(9), (3,3))
s_slice = x[:1,:] # Pure slice
s_scalar = x[0] # Scalar selection
s_numeric = x[[0],:] # Numeric indexing
s_logical = x[array([True,False,False]),:] # Logical indexing
s_logical[0,0] = -40
s_numeric[0,0] = -30
s_numeric # -30
s_logical # -40, not -30
s_scalar[0] = -10
s_scalar
x # Has a -10
s_slice # Has a -10

x = arange(-2,2.0)
x
x[0] = 999 # Scalar
x
x[:2] = array([99.0,99]) # Slice
x
x[[0,1,2]] = array([-3.14,-3.14,-3.14]) # Numerical indexing
x
x[x<0] = zeros(3) # Logical indexing

x = arange(-2,2.0)
x[:2] = 99.0
x
x = log(x-2.0)
x
x[isnan(x)] = 0 # Logical indexing
x
x.shape = (2,2)
x[:,:] = 3.14 # Could also use x[:]
x

x = reshape(arange(-10,10.0),(4,5))
x[sum(x,1)<0,:] = arange(5.0) # Replace rows w/ negative sum
x = reshape(arange(-10,10.0),(4,5))
try:
    x[:,sum(x,1)<0] = arange(4.0) # Error
except:
    print("Error detected in: x[:,sum(x,1)<0] = arange(4.0) # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))
x[:,sum(x,1)<0] = reshape(arange(4.0),(4,1)) # Correct col replacement

