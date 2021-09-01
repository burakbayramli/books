# Solves matrix equation by the Gauss-Jordan method
from random import *
from linsys import *
from matutil import *

n = 5                                                       # order of system
a = [[0]*(n+1) for i in range(n+1)]                           # system matrix
b = [[0]*(n+1) for i in range(n+1)]             # constant terms and solution
c = [[0]*(n+1) for i in range(n+1)]                   # copy of system matrix
d = [[0]*(n+1) for i in range(n+1)]                              # work array

for i in range(1,n+1):                                        # random matrix
   for j in range(1,n+1): a[i][j] = random()
print("A:")
MatPrint(a,n,n)

for i in range(1,n+1):                                          # unit matrix
   for j in range(1,n+1): b[i][j] = 0e0
   b[i][i] = 1e0
print("B:")
MatPrint(b,n,n)

MatCopy(a,c,n,n)                                         # save system matrix

det = GaussJordan(a,b,n,n)                       # solve system, inverse in a

print("Check A^(-1) - X = 0:")
MatDiff(a,b,d,n,n)                  # difference between inverse and solution
MatPrint(d,n,n)

print("Check A^(-1)A = I:")
MatProd(a,c,d,n,n,n)                         # multiply inverse with original
MatPrint(d,n,n)
