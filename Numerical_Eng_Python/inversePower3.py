## module inversePower3
''' lam,x = inversePower3(d,c,s,tol=1.0e-6).
    Inverse power method applied to a tridiagonal matrix
    [A] = [c\d\c]. Returns the eigenvalue closest to 's'
    and the corresponding eigenvector.
'''
from numarray import dot,zeros,Float64
from LUdecomp3 import *
from math import sqrt
from random import random

def inversePower3(d,c,s,tol=1.0e-6):
    n = len(d)
    e = c.copy()
    cc = c.copy()               # Save original [c]
    dStar = d - s               # Form [A*] = [A] - s[I]
    LUdecomp3(cc,dStar,e)       # Decompose [A*]
    x = zeros((n),type=Float64)
    for i in range(n):          # Seed [x] with random numbers
        x[i] = random()
    xMag = sqrt(dot(x,x))       # Normalize [x]
    x =x/xMag
    flag = 0
    for i in range(30):         # Begin iterations    
        xOld = x.copy()         # Save current [x]
        LUsolve3(cc,dStar,e,x)  # Solve [A*][x] = [xOld]
        xMag = sqrt(dot(x,x))   # Normalize [x]
        x = x/xMag
        if dot(xOld,x) < 0.0:   # Detect change in sign of [x]
            sign = -1.0
            x = -x
        else: sign = 1.0
        if sqrt(dot(xOld - x,xOld - x)) < tol:
            return s + sign/xMag,x
    print 'Inverse power method did not converge'

    
    
