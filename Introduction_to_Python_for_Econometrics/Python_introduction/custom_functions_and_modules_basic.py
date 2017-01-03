from __future__ import division
from __future__ import print_function
from __future__ import print_function, division
from core.crosssection.regression import *
import core
import numpy as np
import sys
from pylab import *
from numpy import *
# End Imports


def square(x):
    return x**2
# Call the function
x = 2
y = square(x)
print(x,y)

def l2distance(x,y):
    return (x-y)**2
# Call the function
x = 3
y = 10
z = l2distance(x,y)
print(x,y,z)

def l2_norm(x,y):
    d = x - y
    return np.sqrt(np.dot(d,d))
# Call the function
x = np.random.randn(10)
y = np.random.randn(10)
z = l2_norm(x,y)
print(x-y)
print("The L2 distance is ",z)

def l1_l2_norm(x,y):
    d = x - y
    return sum(np.abs(d)),np.sqrt(np.dot(d,d))
# Call the function
x = np.random.randn(10)
y = np.random.randn(10)
# Using 1 output returns a tuple
z = l1_l2_norm(x,y)
print(x-y)
print("The L1 distance is ",z[0])
print("The L2 distance is ",z[1])
# Using 2 output returns the values
l1,l2 = l1_l2_norm(x,y)
print("The L1 distance is ",l1)
print("The L2 distance is ",l2)

def lp_norm(x,y,p):
    d = x - y
    return sum(abs(d)**p)**(1/p)
# Call the function
x = np.random.randn(10)
y = np.random.randn(10)
z1 = lp_norm(x,y,2)
z2 = lp_norm(p=2,x=x,y=y)
print("The Lp distances are ",z1,z2)

def lp_norm(x,y,p = 2):
    d = x - y
    return sum(abs(d)**p)**(1/p)
# Call the function
x = np.random.randn(10)
y = np.random.randn(10)
# Inputs with default values can be ignored
l2 = lp_norm(x,y)
l1 = lp_norm(x,y,1)
print("The l1 and l2 distances are ",l1,l2)
print("Is the default value overridden?", sum(abs(x-y))==l1)

def bad_function(x = zeros(1)):
    print(x)
    x[0] = np.random.randn(1)
# Call the function
bad_function()
bad_function()
bad_function()

def good_function(x = None):
    if x is None:
        x = zeros(1)
    print(x)
    x[0] = np.random.randn(1)
# Call the function
good_function()
good_function()

def lp_norm(x,y,p = 2, *arguments):
    d = x - y
    print('The L' + str(p) + ' distance is :', sum(abs(d)**p)**(1/p))
    out = [sum(abs(d)**p)**(1/p)]
    for p in arguments:
        print('The L' + str(p) + ' distance is :', sum(abs(d)**p)**(1/p))
        out.append(sum(abs(d)**p)**(1/p))
    return tuple(out)
# Call the function
x = np.random.randn(10)
y = np.random.randn(10)
# Inputs with default values can be ignored
lp = lp_norm(x,y,1,2,3,4,1.5,2.5,0.5)
# Additional arguments are optional
lp = lp_norm(x,y,1)
# Use default for p
lp = lp_norm(x,y)

def lp_norm(x,y,p = 2, **keywords):
    d = x - y
    for key in keywords:
        print('Key :', key, ' Value:', keywords[key])
    return sum(abs(d)**p)
# Call the function
x = np.random.randn(10)
y = np.random.randn(10)
# Inputs with default values can be ignored
lp = lp_norm(x,y,kword1=1,kword2=3.2)
# The p keyword is in the function def, so not in **keywords
lp = lp_norm(x,y,kword1=1,kword2=3.2,p=0)

def lp_norm(x,y,p = 2):
    """ The docstring contains any available help for
        the function.  A good docstring should explain the
        inputs and the outputs, provide an example and a list
        of any other related function.
    """
    d = x - y
    return sum(abs(d)**p)

help(lp_norm)


a, b, c = 1, 3.1415, 'Python'
def scope():
    print(a)
    print(b)
    print(c)
    # print(d) #Error, d has not be declared yet
scope()
d = np.array(1)
def scope2():
    print(a)
    print(b)
    print(c)
    print(d) # Ok now
scope2()
def scope3():
    a = 'Not a number' # Local variable
    print('Inside scope3, a is ', a)
print('a is ',a)
scope3()
print('a is now ',a)

a = 1
def scope_local():
    a = -1
    print('Inside scope_local, a is ',a)



def scope_global():
    global a
    a = -10
    print('Inside scope_global, a is ',a)

print('a is ',a)
scope_local()
print('a is now ',a)
scope_global()
print('a is now ',a)

nested = [('John','Doe','Oxford'),\
          ('Jane','Dearing','Cambridge'),\
          ('Jerry','Dawn','Harvard')]
nested.sort()
nested
nested.sort(key=lambda x:x[1])
nested

r"""Demonstration module
"""
def square(x):
    r"""Returns the square of a scalar input
    """
    return x*x
def cube(x):
    r"""Returns the cube of a scalar input
    """
    return x*x*x

y = -3
print(core.square(y))
print(core.cube(y))

def square(x):
    return x**2
if __name__=="__main__":
    print('Program called directly.')
else:
    print('Program called indirectly using name: ', __name__)


sys.path

