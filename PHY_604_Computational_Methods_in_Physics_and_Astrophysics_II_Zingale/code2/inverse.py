# find the inverse of a matrix using Gaussian elimination

from __future__ import print_function
import numpy as np

def inverse(A_input):
    """ return the inverse of A_input """

    A = A_input.copy()

    N = len(A[:,0])

    # A is square, with each dimension of length N
    if not A.shape == (N, N):
        print("ERROR: A should be square")
        return None

    # create an identity matrix
    I = np.identity(N)

    # allocation for the inverse
    Ainv = np.zeros_like(A)

    # find the scale factors for each row -- this is used when pivoting
    scales = np.max(np.abs(A), 1)

    # keep track of the number of times we swapped rows
    num_row_swap = 0

    # we are essentially doing Gaussian elimination, but with A Ainv = I
    # each column of I represents a separate righthand side to an Ax = b
    # linear system

    # main loop over rows
    for k in range(N):

        # find the pivot row based on the size of column k -- only consider
        # the rows beyond the current row
        row_max = np.argmax(A[k:, k]/scales[k:])
        if k > 0: row_max += k  # we sliced A from k:, correct for total rows

        # swap the row with the largest scaled element in the current column
        # with the current row (pivot) -- do this with b too!
        if not row_max == k:
            A[[k, row_max],:] = A[[row_max, k],:]
            I[[k, row_max],:] = I[[row_max, k],:]
            num_row_swap += 1

        # do the forward-elimination for all rows below the current
        for i in range(k+1, N):
            coeff = A[i,k]/A[k,k]

            for j in range(k+1, N):
                A[i,j] += -A[k,j]*coeff

            A[i,k] = 0.0
            I[i,:] += -coeff*I[k,:]



    # back-substitution -- once for each column in the I matrix

    for c in range(N):

        # last solution is easy
        Ainv[N-1,c] = I[N-1,c]/A[N-1,N-1]

        for i in reversed(range(N-1)):
            sum = I[i,c]
            for j in range(i+1,N):
                sum += -A[i,j]*Ainv[j,c]
            Ainv[i,c] = sum/A[i,i]

    return Ainv


# output: np.savetxt("test.out", a, fmt="%5.2f", delimiter="  ")
# convert -font Courier-New-Regular -pointsize 20 text:test.out test.png


A = np.array([ [1, 1, 1], [-1, 2, 0], [2, 0, 1] ], dtype=np.float64)
Ainv = inverse(A)
print("A . Ainv = \n", np.dot(A, Ainv))

print(" ")

A = np.array([ [0, 1, 1], [1, 1, 0], [1, 0, 1] ], dtype=np.float64)
Ainv = inverse(A)
print("A . Ainv = \n", np.dot(A, Ainv))

print(" ")

A = np.array([ [0, 0, 0, 4],
                  [0, 0, 3, 0],
                  [5, 6, 7, 8],
                  [0, 4, 3, 2] ], dtype=np.float64)
Ainv = inverse(A)
print("A . Ainv = \n", np.dot(A, Ainv))

