# Solves linear system by Gauss elimination
from linsys import *
from matutil import *

n = 4                                                       # order of system
m = 1                                            # number of constant vectors
a = [[0]*(n+1) for i in range(n+1)]                           # system matrix
b = [[0]*(m+1) for i in range(n+1)]                          # constant terms

a[1][1] = 1; a[1][2] = 2; a[1][3] = 3; a[1][4] = 4; b[1][1] = 30
a[2][1] = 2; a[2][2] = 1; a[2][3] = 2; a[2][4] = 3; b[2][1] = 22
a[3][1] = 3; a[3][2] = 2; a[3][3] = 1; a[3][4] = 2; b[3][1] = 18
a[4][1] = 4; a[4][2] = 3; a[4][3] = 2; a[4][4] = 1; b[4][1] = 20
                                  # Solution: 1.0, 2.0, 3.0, 4.0; det = -20.0
print("A:")
MatPrint(a,n,n)
print("B:")
MatPrint(b,n,m)

det = Gauss(a,b,n,m)                                           # solve system

print("det A = ",det)
print("Solution:")
MatPrint(b,n,m)
