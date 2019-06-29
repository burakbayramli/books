import pylab


def ludecomp(A):
    """ Use Crout's algorithm to perform LU decomposition of A. """

    n = len(A)
    L = pylab.zeros(A.shape)
    U = pylab.zeros(A.shape)
    for i in range(n):
        L[i,i] = 1.0
    for j in range(n):
        for i in range(j+1):
            U[i,j] = A[i,j]
            for k in range(i):
                U[i,j] -= L[i,k]*U[k,j]
        for i in range(j+1, n):
            L[i,j] = A[i,j]
            for k in range(j):
                L[i,j] -= L[i,k]*U[k,j]
            L[i,j] /= U[j,j]
    return (L, U)


def determinant(A):
    """ Computes the determinant of a matrix. """

    (L, U) = ludecomp(A)
    det = U[0,0]
    for i in range(1, len(A)):
        det *= U[i,i]
    return det


def solve(A, b):
    """ Solves the linear system A.x = b for x. """

    n = len(A)
    (L, U) = ludecomp(A)
    x = pylab.zeros(b.shape)
    y = pylab.zeros(b.shape)
    # forward substitute to solve equation L.y = b for y
    for i in range(n):
        y[i] = b[i]
        for j in range(i):
            y[i] -= L[i,j]*y[j]
        y[i] /= L[i,i]
    # back substitute to solve equation U.x = y for x
    for i in reversed(range(n)):
        x[i] = y[i]
        for j in range(i+1, n):
            x[i] -= U[i,j]*x[j]
        x[i] /= U[i,i]
    return x


def inverse(A):
    """ Finds the inverse of A. """

    # note that the routine solve works even if b is a matrix!
    B = pylab.eye(len(A))  # the identity matrix
    return solve(A, B)


def tridiag(alp, bet, gam, b):
    """ Solves the linear system A.x = b for x where A is a tridiagonal
    matrix with subdiagonal elements given in the vector alp, diagonal
    elements given in the vector bet, and superdiagonal elements given in
    the vector gam. """

    n = len(bet)
    x = pylab.zeros(b.shape)
    y = pylab.zeros(b.shape)
    y[0] = gam[0]/bet[0]
    x[0] = b[0]/bet[0]
    for i in range(1, n):
        den = bet[i]-alp[i]*y[i-1]
        y[i] = gam[i]/den
        x[i] = (b[i]-alp[i]*x[i-1])/den
    for i in reversed(range(n-1)):
        x[i] -= x[i+1]*y[i]
    return x
