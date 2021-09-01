# Checks identity (A B)_trans = B_trans A_trans for random arrays A and B
from random import *
from matutil import *

# main

n = int(input("n = "))

A = [[0]*(n+1) for i in range(n+1)]                           # define arrays
B = [[0]*(n+1) for i in range(n+1)]
C = [[0]*(n+1) for i in range(n+1)]
D = [[0]*(n+1) for i in range(n+1)]

for i in range(1,n+1):                 # array A: random sub-unitary elements
   for j in range(1,n+1): A[i][j] = random()
print("Array A:")
MatPrint(A,n,n)

for i in range(1,n+1):                 # array B: random sub-unitary elements
   for j in range(1,n+1): B[i][j] = random()
print("Array B:")
MatPrint(B,n,n)

MatProd(A,B,C,n,n,n)                                                    # A*B
MatTrans(C,n)                                                   # (A*B)_trans

MatTrans(A,n)                                                       # A_trans
MatTrans(B,n)                                                       # B_trans
MatProd(B,A,D,n,n,n)                                       # B_trans* A_trans

MatDiff(C,D,D,n,n)                          # (A*B)_trans - B_trans * A_trans
print("Norm ((A*B)_trans - B_trans * A_trans) = ",MatNorm(D,n,n))
