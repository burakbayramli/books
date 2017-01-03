## module fletcherReeves
''' xMin,nIter = optimize(F,gradF,x,h=0.01,tol=1.0e-6
    Fletcher-Reeves method of minimizing a function.
    F(x)     = user-supplied function to be minimized.
    gradF(x) = user-supplied function for grad(F).
    x        = starting point.
    h        = initial search increment used in 'bracket'.
    xMin     = mimimum point.
    nIter    = number of iterations.
'''
from numarray import array,zeros,Float64,dot
from goldSearch import *
from math import sqrt

def optimize(F,gradF,x,h=0.1,tol=1.0e-6):

    def f(s): return F(x + s*v)  # Line function along v

    n = len(x)
    g0 = -gradF(x)
    v = g0.copy()
    F0 = F(x)
    for i in range(200):
        a,b = bracket(f,0.0,h)   # Minimization along 
        s,fMin = search(f,a,b)   # a line
        x = x + s*v
        F1 = F(x)
        g1 = -gradF(x)
        if (sqrt(dot(g1,g1)) <= tol) or (abs(F0 - F1) < tol):
            return x,i+1
        gamma = dot((g1 - g0),g1)/dot(g0,g0)
        v = g1 + gamma*v
        g0 = g1.copy()
        F0 = F1
    print "fletcherReeves did not converge"

