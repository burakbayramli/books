## module eigenvals3
''' lam = eigenvals3(d,c,N).
    Returns the N smallest eigenvalues of a
    tridiagonal matrix [A] = [c\d\c].
'''    
from lamRange import *
from brent import *
from sturmSeq import sturmSeq
from numarray import zeros,Float64

def eigenvals3(d,c,N):

    def f(x):             # f(x) = |[A] - x[I]|
        p = sturmSeq(d,c,x)
        return p[len(p)-1]

    lam = zeros((N),type=Float64)
    r = lamRange(d,c,N)   # Bracket eigenvalues
    for i in range(N):    # Solve by Brent's method
        lam[i] = brent(f,r[i],r[i+1])
    return lam   

