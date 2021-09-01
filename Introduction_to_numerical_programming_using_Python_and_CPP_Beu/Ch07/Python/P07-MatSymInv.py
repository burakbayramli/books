# Inversion of symmetric positive-definite matrix by Cholesky factorization
from random import *
from linsys import *
from matutil import *

n = 100                                                  # order of matrices
a = [[0]*(n+1) for i in range(n+1)]
b = [[0]*(n+1) for i in range(n+1)]
c = [[0]*(n+1) for i in range(n+1)]

for i in range(1,n+1):              # generate random lower triangular matrix
   for j in range(1  ,i+1): a[i][j] = random()               # lower triangle
   for j in range(i+1,n+1): a[i][j] = 0e0                    # upper triangle
   a[i][i] += 1e0                                         # increase diagonal

MatCopy(a,b,n,n)
MatTrans(b,n)                        # create upper triangular matrix b = a^T
MatProd(a,b,c,n,n,n)                  # c = a a^T symmetric positive-definite
MatCopy(c,a,n,n)
MatCopy(c,b,n,n)

det = MatSymInv(a,n)                      # inverse by Cholesky factorization

MatProd(a,b,c,n,n,n)                             # c = a^(-1)a  "unit" matrix
print("A^(-1)A (sample):\n")
MatPrint(c,5,5)                               # print sample of "unit" matrix

for i in range(1,n+1): c[i][i] -= 1e0            # transform to "zero" matrix
err = MatNorm(c,n,n)                                      # max(abs(c[i][j]))
print("\nMaximum error = ","{0:8.2e}".format(err))
