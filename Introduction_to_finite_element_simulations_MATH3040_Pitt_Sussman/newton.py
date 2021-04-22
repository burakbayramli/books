import numpy as np
import scipy.linalg as la
import copy

def ex1(x):
    """
    intersection of circle and parabola
    """
    assert(len(x) == 2)
    f=np.empty(2)
    fprime=np.empty([2,2])
    f[0]=x[0]**2+x[1]**2-1.0
    f[1]=x[1]-x[0]**2
    fprime[0,0]=2.0*x[0]
    fprime[0,1]=2.0*x[1]
    fprime[1,0]=-2.0*x[0]
    fprime[1,1]=1.0
    return f,fprime
    

def newton(f,xin):
    """
    Newton's method
    the function f returns the pair (f,fprime)
    """
    EPSILON = 1.0e-10
    x = copy.deepcopy(xin)
    for n in range(100):   # usually converges in 100 iterations
        value,derivative = f(x)
        increment = la.solve(derivative,value)
        x -= increment
        errorEstimate = la.norm(increment)/la.norm(x)
        print "errorEstimate = ",errorEstimate
        if errorEstimate < EPSILON:
            return x,n
    assert(False)
    
def ex2(x):
    """
    intersection of circle and parabola
    with mistake in Jacobian
    """
    assert(len(x) == 2)
    f=np.empty(2)
    fprime=np.empty([2,2])
    f[0]=x[0]**2+x[1]**2-1.0
    f[1]=x[1]-x[0]**2
    fprime[0,0]=2.0*x[0]
    fprime[0,1]=2.0*x[1]
    fprime[1,0]=-2.0*x[0]
    fprime[1,1]=x[1]  # WRONG!
    return f,fprime


y,i = newton(ex1,np.array([10.,10.]))
print "y = ",y," i= ",i

y,i = newton(ex2,np.array([10.,10.]))
print "y = ",y," i= ",i