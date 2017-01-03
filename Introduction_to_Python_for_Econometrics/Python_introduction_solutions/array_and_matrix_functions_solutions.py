"""Solutions for 'Special Arrays' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('special_arrays_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function, division
from numpy import arange, array, reshape, hstack, vstack, tile, \
    diag, dot, kron, trace, prod, eye
from numpy.linalg import eig, eigvals, cholesky, det, inv, lstsq, matrix_rank
from numpy.random import randn
# <demo> --- stop ---
# Exercise 1
x = arange(12.0)
x.shape = 2,6
x.shape = 3,4
x.shape = 4,3
x.shape = 6,2
x.shape = 2,2,3
x.shape = 12
# <demo> --- stop ---
# Exercise 2
x = reshape(arange(12.0),(4,3))
print("x.flatten()[1::2]:")
print(x.flatten()[1::2])
print("x.ravel()[1::2]:")
print(x.ravel()[1::2])
print("x.flat[1::2]:")
print(x.flat[1::2])
# <demo> --- stop ---
# Exercise 3
x = randn(2,2)
y = randn(1,1)
z = randn(3,2)

pt1 = hstack((x,tile(y,(2,3))))
pt2 = vstack((z.T,tile(y,(1,3))))
pt3 = hstack((z,pt2))
final = vstack((pt1,pt3))
# <demo> --- stop ---
# Exercise 4
x = reshape(arange(12.0),(2,2,3))
print("x.squeeze():")
print(x.squeeze())
# <demo> --- stop ---
# Exercise 5
y = array([[2,0.5],[0.5,4]])
z = diag(diag(y))
# <demo> --- stop ---
# Exercise 6
z = cholesky(y)
print("dot(z,z.T):")
print(dot(z,z.T))
# <demo> --- stop ---
# Exercise 7
print("trace(y):")
print(trace(y))
D = eigvals(y)
print("sum(D):")
print(sum(D))
# <demo> --- stop ---
# Exercise 7b
print("det(y):")
print(det(y))
print("prod(D):")
print(prod(D))
# <demo> --- stop ---
# Exercise 8
print("inv(y):")
print(inv(y))
(D,V) = eig(y)
D = 1/D
print("dot(dot(V,diag(D)),V.T):")
print(dot(dot(V,diag(D)),V.T))
# <demo> --- stop ---
# Exercise 9
x = randn(100,2)
e = randn(100,1)
B = array([[1],[0.5]])
y = dot(x,B) + e

out = lstsq(x,y)
estimate = out[0]
# <demo> --- stop ---
# Exercise 10
y=array([[5,-1.5,-3.5],[-1.5,2,-0.5],[-3.5,-0.5,4]])
D = eigvals(y)
print("matrix_rank(y):")
print(matrix_rank(y))
print("det(y):")
print(det(y))
# <demo> --- stop ---
# Exercise 11
x = randn(100,2)
SigmaX = dot(x.T,x)/100
print("kron(eye(2),SigmaX):")
print(kron(eye(2),SigmaX))
