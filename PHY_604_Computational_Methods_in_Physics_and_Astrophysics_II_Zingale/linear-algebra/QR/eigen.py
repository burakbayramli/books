# Construct the QR decomposition of a real, symmetric matrix A and use
# this to find the eigenvalues and eigenvectors.
#
# We follow the procedure outlined in Newman section 6.2

from __future__ import print_function

import numpy as np


def get_QR(A):
    """ given A, return the matrices Q and R, such that Q
        is an orthogonal matrix and R is upper-triangular """

    N = A.shape[0]
    if not A.shape == (N, N):
        print("ERROR: matrix A must be square")
        return None

    Q = np.zeros_like(A)
    R = np.zeros_like(A)

    q = []
    u = []

    # create the orthonormal basis vectors from the
    # columns of A -- note we need to copy A here, since
    # just indexing a column gives a view into the array
    # A, and we'd be changing A as we alter u
    u.append(np.copy(A[:,0]))
    q.append(u[0]/np.sqrt(u[0].dot(u[0])))

    for i in range(1, N):
        u.append(np.copy(A[:,i]))
        for j in range(i):
            u[i] -= q[j].dot(A[:,i])*q[j]

        q.append(u[i]/np.sqrt(u[i].dot(u[i])))

    # create Q
    for n in range(N):
        Q[:,n] = q[n]

    # create R
    for i in range(N):
        for j in range(i,N):
            if i == j:
                R[i,j] = np.sqrt(u[i].dot(u[i]))
            else:
                R[i,j] = q[i].dot(A[j])

    return Q, R


def eigen(A, eps=1.e-6):
    """ use the QR algorithm for a real, symmetric matrix to geth
    the eigenvalues and eigenvectors """

    N = A.shape[0]
    if not A.shape == (N, N):
        print("ERROR: matrix A must be square")
        return None

    V = np.eye(N)

    err = 1.e10
    A_old = np.copy(A)
    while err > eps:
        Q, R = get_QR(A_old)
        A_new = R.dot(Q)
        V = V.dot(Q)

        A2 = A_new**2  # element-by-element
        err = np.sqrt(np.sum(A2) - np.trace(A2))   # this is kinda an L2 norm of the off-diagonals

        A_old = A_new

    return A_new, V


A = np.array([[1, 4, 8, 4],
              [4, 2, 3, 7],
              [8, 3, 6, 9],
              [4, 7, 9, 2]], dtype=np.float64)


Lambda, V = eigen(A, eps=1.e-10)

for n in range(A.shape[0]):
    print("eigenvalue: {}".format(Lambda[n,n]))
    print("    eigenvector:    {}".format(V[:,n]))
    print("    A v - lambda v: {}\n".format(A.dot(V[:,n]) - Lambda[n,n]*V[:,n]))
