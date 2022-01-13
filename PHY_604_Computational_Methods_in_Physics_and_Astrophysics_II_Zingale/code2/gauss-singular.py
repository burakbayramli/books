import numpy
from gauss import *
from matmul import *


# this matrix is singular -- watch out!!!
A = numpy.array([ [ 1, 2, 3], 
                  [ 4, 5, 6], 
                  [ 7, 8, 9] ], dtype=numpy.float64)
b = numpy.array([5, -1, 2], dtype=numpy.float64)
x, det = gauss_elim(A.copy(), b.copy(), return_det=1)

# test it by multiplying A x
bout = mult_Ax(A, x)

print("matrix A:\n", A)
print("RHS (b): ", b)
print("solved x: ", x)
print("A.x: ", bout)
print("det: ", det)
print(" ")
