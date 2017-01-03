## module inversePower5
''' lam,x = inversePower5(Bv,d,e,f,tol=1.0e-6).
    Inverse power method for solving the eigenvalue problem
    [A]{x} = lam[B]{x}, where [A] = [f\e\d\e\f] is
    pentadiagonal and [B] is sparce. User must supply the
    function Bv(v) that returns the vector [B]{v}.
'''
from numarray import zeros,Float64,dot
from LUdecomp5 import *
from math import sqrt
from random import random

def inversePower5(Bv,d,e,f,tol=1.0e-6):  
    n = len(d)
    d,e,f = LUdecomp5(d,e,f)
    x = zeros((n),type=Float64)
    for i in range(n):          # Seed {v} with random numbers
        x[i] = random()
    xMag = sqrt(dot(x,x))       # Normalize {v}
    x = x/xMag
    for i in range(30):         # Begin iterations     
        xOld = x.copy()         # Save current {v}
        x = Bv(xOld)            # Compute [B]{v}
        x = LUsolve5(d,e,f,x)   # Solve [A]{z} = [B]{v}
        xMag = sqrt(dot(x,x))   # Normalize {z}
        x = x/xMag
        if dot(xOld,x) < 0.0:   # Detect change in sign of {x}
            sign = -1.0
            x = -x
        else: sign = 1.0
        if sqrt(dot(xOld - x,xOld - x)) < tol:
            return sign/xMag,x
    print 'Inverse power method did not converge'
