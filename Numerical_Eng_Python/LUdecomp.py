## module LUdecomp
''' a = LUdecomp(a).
    LU decomposition: [L][U] = [a]. The returned matrix [a] = [L\U]
    contains [U] in the upper triangle and the nondiagonal terms
    of [L] in the lower triangle.

    x = LUsolve(a,b).
    Solves [L][U]{x} = b, where [a] = [L\U] is the matrix returned
    from LUdecomp.
'''
from numarray import dot

def LUdecomp(a):
    n = len(a)
    for k in range(0,n-1):
        for i in range(k+1,n):
           if a[i,k] != 0.0:
               lam = a [i,k]/a[k,k]
               a[i,k+1:n] = a[i,k+1:n] - lam*a[k,k+1:n]
               a[i,k] = lam
    return a

def LUsolve(a,b):
    n = len(a)
    for k in range(1,n):
        b[k] = b[k] - dot(a[k,0:k],b[0:k])  
    for k in range(n-1,-1,-1):
       b[k] = (b[k] - dot(a[k,k+1:n],b[k+1:n]))/a[k,k]
    return b

