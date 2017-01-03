"""Solutions for 'Special Arrays' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('special_arrays_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
from numpy import zeros, ones, empty, dot, eye, exp, tile, array, diag
# <demo> --- stop ---
# Exercise 1
x = zeros((10,5))
y = ones((10,5))
# <demo> --- stop ---
# Exercise 2
print("dot(x,y.T):")
print(dot(x,y.T))
print("dot(x.T,y):")
print(dot(x.T,y))
# <demo> --- stop ---
# Exercise 3
z = eye(5)
print("exp(z):")
print(exp(z))
# <demo> --- stop ---
# Exercise 4
x = tile(array([0.0]),(10,5))
y = tile(array([1.0]),(10,5))
# <demo> --- stop ---
# Exercise 5
z = diag(ones((5)))
# <demo> --- stop ---
# Exercise 6
y = empty((1,))
y = empty((10,))