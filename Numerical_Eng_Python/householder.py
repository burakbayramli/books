## module householder
''' d,c = householder(a).
    Householder similarity transformation of matrix [a] to 
    the tridiagonal form [c\d\c].

    p = computeP(a).
    Computes the acccumulated transformation matrix [p]
    after calling householder(a).
'''    
from numarray import dot,matrixmultiply,diagonal, \
     outerproduct,identity
from math import sqrt

def householder(a): 
    n = len(a)
    for k in range(n-2):
        u = a[k+1:n,k]
        uMag = sqrt(dot(u,u))
        if u[0] < 0.0: uMag = -uMag
        u[0] = u[0] + uMag
        h = dot(u,u)/2.0
        v = matrixmultiply(a[k+1:n,k+1:n],u)/h
        g = dot(u,v)/(2.0*h)
        v = v - g*u
        a[k+1:n,k+1:n] = a[k+1:n,k+1:n] - outerproduct(v,u) \
                         - outerproduct(u,v)
        a[k,k+1] = -uMag
    return diagonal(a),diagonal(a,1)

def computeP(a): 
    n = len(a)
    p = identity(n)*1.0
    for k in range(n-2):
        u = a[k+1:n,k]
        h = dot(u,u)/2.0
        v = matrixmultiply(p[1:n,k+1:n],u)/h           
        p[1:n,k+1:n] = p[1:n,k+1:n] - outerproduct(v,u)
    return p
      
                

