# Eigenvalues and eigenvectors of inverse of symmetric matrix
from random import *
from linsys import *
from eigsys import *
from matutil import *

n = 100                                                     # order of matrix
a  = [[0]*(n+1) for i in range(n+1)]                     # coefficient matrix
a1 = [[0]*(n+1) for i in range(n+1)]                           # inverse of a
x  = [[0]*(n+1) for i in range(n+1)]                      # eigenvectors of a
x1 = [[0]*(n+1) for i in range(n+1)]                     # eigenvectors of a1
d = [0]*(n+1); d1 = [0]*(n+1)                       # eigenvalues of a and a1

seed()                                   # initialize random number generator
for i in range(1,n+1):                   # generate random coefficient matrix
   for j in range(1,i+1):
      a[i][j] = a[j][i] = random()
      a1[i][j] = a1[j][i] = a[i][j]                            # copy a in a1

Jacobi(a,x,d,n)                              # solve eigenvalue problem for a
EigSort(x,d,n,1)                          # sort eigenvalues and eigenvectors

MatInv(a1,n)                                                    # a1 = a^(-1)
Jacobi(a1,x1,d1,n)                      # solve eigenvalue problem for a^(-1)
for i in range(1,n+1): d1[i] = 1e0/d1[i]  # invert eigenvalues before sorting
EigSort(x1,d1,n,1)                        # sort eigenvalues and eigenvectors

for j in range(1,n+1):                               # loop over eigenvectors
   if (x[1][j] * x1[1][j] < 0):           # eigenvectors have different sign?
      for i in range(1,n+1): x1[i][j] = -x1[i][j]                # match sign

VecDiff(d,d1,d1,n)                          # difference of eigenvalues in d1
norm = VecNorm(d1,n)
print("Norm of eigenvalue difference = {0:8.2e}".format(norm))

MatDiff(x,x1,x1,n,n)                       # difference of eigenvectors in x1
norm = MatNorm(x1,n,n)
print("Norm of eigenvector difference = {0:8.2e}".format(norm))
