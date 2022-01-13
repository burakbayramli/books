from __future__ import print_function

import numpy as np

def tridiag(a, b, c, d):
    """ solve the linear system Ax = d where A has the form:

          a_i x_{i-1} + b_i x_i + c_i x_{i+1} = d_i

        for i = 0, n-1 with a_0 = 0 and c_{n-1} = 0

        In matrix form, b is the main diagonal, a is the subdiagonal and
        c is the superdiagonal. """

    N = len(a)
    if not len(b) == len(c) == len(d) == N:
        print("ERROR: vectors not the right size")
        return None

    # forward elimination
    cprime = np.zeros((N), dtype=a.dtype)
    dprime = np.zeros((N), dtype=a.dtype)
    
    cprime[0] = c[0]/b[0]
    dprime[0] = d[0]/b[0]

    for i in range(1,N-1):
        cprime[i] = c[i]/(b[i] - cprime[i-1]*a[i])
        dprime[i] = (d[i] - dprime[i-1]*a[i])/(b[i] - cprime[i-1]*a[i])
        
    dprime[N-1] = (d[N-1] - dprime[N-2]*a[N-1])/(b[N-1] - cprime[N-2]*a[N-1])

    # back substitution
    x = np.zeros((N), dtype=a.dtype)

    x[N-1] = dprime[N-1]
    for i in reversed(range(0,N-1)):
        x[i] = dprime[i] - cprime[i]*x[i+1]

    return x


def tridiag_Ax(a, b, c, x):
    """ multiply the tridiagonal matrix A by vector x and return the
        product vector d """

    N = len(a)
    if not len(b) == len(c) == len(x) == N:
        print("ERROR: vectors not the right size")
        return None
    
    d = np.zeros((N), dtype=a.dtype)

    d[0] = b[0]*x[0] + c[0]*x[1]
    for i in range(1,N-1):
        d[i] = a[i]*x[i-1] + b[i]*x[i] + c[i]*x[i+1]
    
    d[N-1] = a[N-1]*x[N-2] + b[N-1]*x[N-1]

    return d


    
    
