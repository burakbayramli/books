"""Solutions for 'Basic Functions' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('basic_functions_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function, division
import numpy as np
from numpy import linspace, arange, r_, logspace, array, log10, around, \
    ceil, floor, cumsum, diff, exp, sort, absolute, logical_not, nansum, \
    union1d, in1d, intersect1d, nan, isnan, log, hstack    
from numpy.random import randn
# <demo> --- stop ---
# Exercise 1
print("linspace(0,10,11):")
print(linspace(0,10,11))
print("arange(11):")
print(arange(11))
print("r_[0:11]:")
print(r_[0:11])

print("linspace(4,13,10):")
print(linspace(4,13,10))
print("arange(4,14):")
print(arange(4,14))
print("r_[4:14]:")
print(r_[4:14])

print("linspace(0,1,5):")
print(linspace(0,1,5))
print("arange(0,1.01,.25):")
print(arange(0,1.01,.25))
print("r_[0:1:5j]:")
print(r_[0:1:5j])

print("linspace(0,-5,5):")
print(linspace(0,-5,5))
print("arange(0,-6,-1):")
print(arange(0,-6,-1))
print("r_[0:-6:-1]:")
print(r_[0:-6:-1])
# <demo> --- stop ---
# Exercise 2
print("logspace(0,2,21) :")
print(logspace(0,2,21) )
print("10**linspace(0,2,21):")
print(10**linspace(0,2,21))

print("linspace(2,10,51):")
print(linspace(2,10,51))
print("log10(logspace(2,10,51)):")
print(log10(logspace(2,10,51)))
# <demo> --- stop ---
# Exercise 3
y=array([0,0.5,1.5,2.5,1.0,1.0001,-0.5,-1,-1.5,-2.5])
print("y.round():")
print(y.round())
print("around(y):")
print(around(y))
print("ceil(y):")
print(ceil(y))
print("floor(y):")
print(floor(y))
# <demo> --- stop ---
# Exercise 4
n=10
y = arange(n+1)
print("cumsum(y):")
print(cumsum(y))
print("y*(y+1)/2:")
print(y*(y+1)/2)
# <demo> --- stop ---
# Exercise 5
x = randn(20)
y = cumsum(x)
print("y[19]:")
print(y[19])
print("sum(x):")
print(sum(x))
# <demo> --- stop ---
# Exercise 6
x = randn(10)
y = cumsum(x)
z = hstack((0,y))
print("diff(z):")
print(diff(z))
# <demo> --- stop ---
# Exercise 7
print("exp(array([log(0.5),log(1),log(np.e)])):")
print(exp(array([log(0.5),log(1),log(np.e)])))
# <demo> --- stop ---
# Exercise 8
print("absolute(array([0.0,-3.14,3+4j])):")
print(absolute(array([0.0,-3.14,3+4j])))
# <demo> --- stop ---
# Exercise 9
x = array([-4,2,-9,-8,10])
y = sort(x)
print("x:")
print(x)
print("x.sort():")
print(x.sort())
print("x:")
print(x)
# <demo> --- stop ---
# Exercise 10
print("x.max():")
print(x.max())
i = x.argmax()
print("x[i]:")
print(x[i])
# <demo> --- stop ---
# Exercise 11a
x = array([1,2,3,4,5])
y = array([1,2,4,6])
print("y[logical_not(in1d(y,intersect1d(x,y)))]:")
print(y[logical_not(in1d(y,intersect1d(x,y)))])
# <demo> --- stop ---
# Exercise 11b
z=union1d(x,y)
print("z[logical_not(in1d(z,intersect1d(x,y)))]:")
print(z[logical_not(in1d(z,intersect1d(x,y)))])

y = array([nan,2.2,3.9,4.6,nan,2.4,6.1,1.8])
T = sum(logical_not(isnan(y)))
# <demo> --- stop ---
# Exercise 12
Ey2=nansum(y**2)/T
Ey =nansum(y)/T
nanvar = Ey2-Ey**2