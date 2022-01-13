import numpy as np

def LU(A):
    """return the LU decomposition of a matrix A.  Note: we don't do
    pivoting here, so this will not be general."""

    if A.shape[0] != A.shape[1]:
        raise ValueError("matrix A should be square")

    # we follow Newman here.  We also assume python >= 3.6, so we can
    # use the @ operator

    # we are not going to worry about memory efficiency here, in favor
    # of clarity
    L = np.zeros_like(A)
    U = A.copy()
    L_n = np.zeros_like(A)

    N = A.shape[0]

    for n in range(N):

        # store the current column in our full L
        L[n:,n] = U[n:,n]

        # setup the current L_n
        L_n[:,:] = U[n,n]*np.eye(N)
        L_n[n,n] = 1.0
        L_n[n+1:,n] = -U[n+1:,n]
        L_n[:,:] = L_n[:,:]/U[n,n]

        # U <- L_n U
        U[:,:] = L_n @ U

    return L, U


if __name__ == "__main__":

    A = np.array([[3., -7., -2., 2],
                  [-3., 5., 1.0, 0.],
                  [6., -4., 0, -5.],
                  [-9., 5., -5., 12]])

    L, U = LU(A)
    print(L)
    print(U)
    print(L @ U - A)
    print(" ")

    A = np.array([[4., 3.], [6., 3.]])

    L, U = LU(A)
    print(L)
    print(U)
    print(L @ U - A)
