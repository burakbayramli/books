from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = arange(5)
type(x)
x.view(matrix)
x.view(recarray)

x = array([[1,2],[3,4]])
x * x           # Element-by-element
mat(x) * mat(x) # Matrix multiplication

x = randn(4,3)
x.shape
shape(x)
M,N = shape(x)
x.shape = 3,4
x.shape
x.shape = 6,-1
x.shape

x = array([[1,2],[3,4]])
y = reshape(x,(4,1))
y
z=reshape(y,(1,4))
z
w = reshape(z,(2,2))

x = randn(4,3)
size(x)
x.size

x = randn(4,3)
ndim(x)
x.ndim

x = array([[1,2],[3,4]])
z = hstack((x,x,x))
y = vstack((z,z))

w = tile(x,(2,3))
y - w

x = array([[1,2],[3,4]])
x
x.ravel()
x.T.ravel()

x = array([[1,2],[3,4]])
x.flat
x.flat[2]
x.flat[1:4] = -1
x

x = array([[1,2,3,4]])
y = reshape(x,(4,1))
b = broadcast(x,y)
b.shape
for u,v in b:
    print('x: ', u, ' y: ',v)

x = array([[1,2,3,4]])
y = reshape(x,(4,1))
b = broadcast_arrays(x,y)
b[0]
b[1]

x = reshape(arange(6),(2,3))
y = x
vstack((x,y))
hstack((x,y))

x = reshape(arange(20),(4,5))
y = vsplit(x,2)
len(y)
y[0]
y = hsplit(x,[1,3])
len(y)
y[0]
y[1]

x = reshape(arange(20),(4,5))
delete(x,1,0) # Same as x[[0,2,3]]
delete(x,[2,3],1) # Same as x[:,[0,1,4]]
delete(x,[2,3]) # Same as hstack((x.flat[:2],x.flat[4:]))

x = ones((5,1,5,1))
shape(x)
y = x.squeeze()
shape(y)
y = squeeze(x)

x = reshape(arange(4),(2,2))
x
fliplr(x)
flipud(x)

x = array([[1,2],[3,4]])
x
y = diag(x)
y
z = diag(y)
z

x = array([[1,2],[3,4]])
triu(x)
tril(x)

x = matrix([[1.0,0.5],[.5,1]])
cond(x)
x = matrix([[1.0,2.0],[1.0,2.0]]) # Singular
cond(x)

X = array([[1.0,2.0,3.0],[3.0,3.0,4.0],[1.0,1.0,4.0]])
y = array([[1.0],[2.0],[3.0]])
solve(X,y)

X = randn(100,2)
y = randn(100)
lstsq(X,y)

x = matrix([[1,.5],[.5,1]])
C = cholesky(x)
C*C.T - x

x = matrix([[1,.5],[.5,1]])
det(x)

x = matrix([[1,.5],[.5,1]])
val,vec = eig(x)
vec*diag(val)*vec.T

x = array([[1,.5],[.5,1]])
xInv = inv(x)
dot(x,xInv)
x = asmatrix(x)
x**(-1)*x

x = array([[1,.5],[1,.5]])
x
matrix_rank(x)

