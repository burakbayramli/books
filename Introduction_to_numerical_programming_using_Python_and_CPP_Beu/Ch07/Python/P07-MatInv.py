# Check of matrix inversion using LU decomposition
from random import *
from linsys import *
from matutil import *

n = 5                                                       # order of matrix
a = [[0]*(n+1) for i in range(n+1)]             # original matrix and inverse
b = [[0]*(n+1) for i in range(n+1)]                      # backup of original
c = [[0]*(n+1) for i in range(n+1)]                            # check matrix

for i in range(1,n+1):                               # generate random matrix
   for j in range(1,n+1): a[i][j] = random()

print("Original matrix:")
MatPrint(a,n,n)

MatCopy(a,b,n,n)                                     # backup original matrix

det = MatInv(a,n)                                           # invert original
if (det == 0e0): print("Singular matrix"); exit(1)

print("Inverse matrix:")
MatPrint(a,n,n)

print("Check A^(-1)A = I:")
MatProd(a,b,c,n,n,n)                         # multiply inverse with original
MatPrint(c,n,n)                                          # print check matrix
