from __future__ import print_function

import numpy
from gauss import *
from matmul import *

# example from G. J. Tee, "A Simple Example of An Ill-Conditioned Matrix"

# this matrix is nearly singular
A = numpy.array([ [ 11, 10,  14],
                  [ 12, 11, -13],
                  [ 14, 13, -66] ], dtype=numpy.float64)

b = numpy.array([1.001, 0.999, 1.001], dtype=numpy.float64)

x = gauss_elim(A.copy(), b.copy())

# test it by multiplying A x
bout = mult_Ax(A, x)

print("matrix A:\n", A)
print("RHS (b): ", b)
print("solved x: ", x)
print("A.x: ", bout)
print(" ")


# now change b by only 0.1%
b = numpy.array([1.00, 1.00, 1.00], dtype=numpy.float64)

x = gauss_elim(A.copy(), b.copy())

# test it by multiplying A x
bout = mult_Ax(A, x)

print("matrix A:\n", A)
print("RHS (b): ", b)
print("solved x: ", x)
print("A.x: ", bout)
print(" ")

