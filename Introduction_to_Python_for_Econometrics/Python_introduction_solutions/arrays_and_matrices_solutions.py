"""Solutions for 'Arrays and Matrices' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('arrays_and_matrices_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function, division
import numpy as np
from numpy import array, matrix, shape
# <demo> --- stop ---
# Exercise 1
u = array([1.0,1,2,3,5,8])
v = array([[1.0],[1],[2],[3],[5],[8]])
x = array([[1.0,0],[0,1]])
y = array([[1.0,2],[3,4]])
z = array([[1.0,2,1,2],[3,4,3,4],[1,2,1,3]])
w = array([[1.0,0,1,0],[0,1,0,1],[1,2,1,2],[3,4,3,4]])

u = matrix([1.0,1,2,3,5,8])
v = matrix([[1.0],[1],[2],[3],[5],[8]])
x = matrix([[1.0,0],[0,1]])
y = matrix([[1.0,2],[3,4]])
z = matrix([[1.0,2,1,2],[3,4,3,4],[1,2,1,3]])
w = matrix([[1.0,0,1,0],[0,1,0,1],[1,2,1,2],[3,4,3,4]])
# <demo> --- stop ---
# Exercise 2
print("w[:2,:2]:")
print(w[:2,:2])
# <demo> --- stop ---
# Exercise 3
print("w[:,:2]:")
print(w[:,:2])
print("w[:,-2:]:")
print(w[:,-2:])
# <demo> --- stop ---
# Exercise 4
print("z[:2,:2]:")
print(z[:2,:2])
print("z[:2,2:]:")
print(z[:2,2:])
# <demo> --- stop ---
# Exercise 5
y = array([[1.0,-2],[-3,4]],dtype=np.float)
y = array([[1.0,-2],[-3,4]],dtype=np.float64)
y = array([[1.0,-2],[-3,4]],dtype=np.int32)
y = array([[1.0,-2],[-3,4]],dtype=np.uint32)
y = array([[1.0,-2],[-3,4]],dtype=np.complex128)

y = array([[1.0,-2],[-3,4]],copy=False)

y = array([[1.0,-2],[-3,4]],ndmin=3)
print("shape(y):")
print(shape(y))
y = array([[1.0,-2],[-3,4]],ndmin=4)
print("shape(y):")
print(shape(y))
# <demo> --- stop ---
# Exercise 6
# X is 2-dimensional, y is 1
# Note the [[]] on x
y = array([1.6180, 2.7182, 3.1415])
x = matrix(y)