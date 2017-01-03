## module powell
''' xMin,nCyc = powell(F,x,h=0.1,tol=1.0e-6)
    Powell's method of minimizing user-supplied function F(x).
    x    = starting point
    h   = initial search increment used in 'bracket'
    xMin = mimimum point
    nCyc = number of cycles
'''
from numarray import identity,array,dot,zeros,Float64,argmax
from goldSearch import *
from math import sqrt

def powell(F,x,h=0.1,tol=1.0e-6):
    
    def f(s): return F(x + s*v)    # F in direction of v

    n = len(x)                     # Number of design variables
    df = zeros((n),type=Float64)   # Decreases of F stored here
    u = identity(n)*1.0            # Vectors v stored here by rows
    for j in range(30):            # Allow for 30 cycles:
        xOld = x.copy()            # Save starting point
        fOld = F(xOld)
      # First n line searches record decreases of F
        for i in range(n):
            v = u[i]
            a,b = bracket(f,0.0,h)
            s,fMin = search(f,a,b)
            df[i] = fOld - fMin
            fOld = fMin
            x = x + s*v
      # Last line search in the cycle    
        v = x - xOld
        a,b = bracket(f,0.0,h)
        s,fLast = search(f,a,b)
        x = x + s*v
      # Check for convergence
        if sqrt(dot(x-xOld,x-xOld)/n) < tol: return x,j+1
      # Identify biggest decrease & update search directions
        iMax = int(argmax(df))
        for i in range(iMax,n-1):
            u[i] = u[i+1]
            u[n-1] = v
    print "Powell did not converge"        
        
    

    
