
# Code from Chapter 11 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Gradient Descent using Newton's method
from numpy import *

def Jacobian(x):
    #return array([.4*x[0],2*x[1]])
    return array([x[0], 0.4*x[1], 1.2*x[2]])

def Hessian(x):
    #return array([[.2,0],[0,1]])
    return array([[1,0,0],[0,0.4,0],[0,0,1.2]])

def Newton(x0):

    i = 0
    iMax = 10
    x = x0
    Delta = 1
    alpha = 1
    
    while i<iMax and Delta>10**(-5):
        p = -dot(linalg.inv(Hessian(x)),Jacobian(x))
        xOld = x
        x = x + alpha*p
        Delta = sum((x-xOld)**2)
        i += 1
    print x
    
x0 = array([-2,2,-2])
Newton(x0)
