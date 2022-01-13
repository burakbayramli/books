from __future__ import print_function

import numpy as np
import gauss
import matmul


# tests of gaussian elimination

A = np.array([ [1, 1, 1], [-1, 2, 0], [2, 0, 1] ], dtype=np.float64)
b = np.array([6, 3, 5], dtype=np.float64)

# gaussElim changes A in place -- send a copy
x, d = gauss.gauss_elim(A.copy(), b.copy(), return_det=1)

# test it by multiplying A x
bout = matmul.mult_Ax(A, x)

print(" ")
print("matrix A:\n", A)
print("RHS (b):   ", b)
print("solved x:  ", x)
print("det{A}:    ", d)
print("A.x:       ", bout)
print(" ")

A = np.array([ [0, 1, 1], [1, 1, 0], [1, 0, 1] ], dtype=np.float64)
b = np.array([5, 3, 4], dtype=np.float64)
x = gauss.gauss_elim(A.copy(), b.copy())

# test it by multiplying A x
bout = matmul.mult_Ax(A, x)

print(" ")
print("matrix A:\n", A)
print("RHS (b):   ", b)
print("solved x:  ", x)
print("A.x:       ", bout)
print(" ")


A = np.array([ [0, 0, 0, 4],
                  [0, 0, 3, 0],
                  [5, 6, 7, 8],
                  [0, 4, 3, 2] ], dtype=np.float64)
b = np.array([5, 4, 9, 1], dtype=np.float64)
x = gauss.gauss_elim(A.copy(), b.copy())

# test it by multiplying A x
bout = matmul.mult_Ax(A, x)

print("matrix A:\n", A)
print("RHS (b): ", b)
print("solved x: ", x)
print("A.x: ", bout)
print(" ")


A = np.array([ [ 4,  3, 4, 10],
                  [ 2, -7, 3,  0],
                  [-2, 11, 1,  3],
                  [ 3, -4, 0,  2] ], dtype=np.float64)
b = np.array([2, 6, 3, 1], dtype=np.float64)
x = gauss.gauss_elim(A.copy(), b.copy())

# test it by multiplying A x
bout = matmul.mult_Ax(A, x)

print("matrix A:\n", A)
print("RHS (b): ", b)
print("solved x: ", x)
print("A.x: ", bout)
print(" ")
