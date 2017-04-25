## module LUdecomp
''' a = LUdecomp(a).
    LU decomposition: [L][U] = [a]. The returned matrix [a] = [L\U]
    contains [U] in the upper triangle and the nondiagonal terms
    of [L] in the lower triangle.

    x = LUsolve(a,b).
    Solves [L][U]{x} = b, where [a] = [L\U] is the matrix returned
    from LUdecomp.
'''
from numpy import dot

def LUdecomp(a):
    n = len(a)
    for k in range(0,n-1):
        for i in range(k+1,n):
           if abs(a[i,k]) > 1.0e-9:
               lam = a [i,k]/a[k,k]
               a[i,k+1:n] = a[i,k+1:n] - lam*a[k,k+1:n]
               a[i,k] = lam
    return a

def LUsolve(a,b):
    n = len(a)
    for k in range(1,n):
        b[k] = b[k] - dot(a[k,0:k],b[0:k])
    b[n-1] = b[n-1]/a[n-1,n-1]    
    for k in range(n-2,-1,-1):
       b[k] = (b[k] - dot(a[k,k+1:n],b[k+1:n]))/a[k,k]
    return b

