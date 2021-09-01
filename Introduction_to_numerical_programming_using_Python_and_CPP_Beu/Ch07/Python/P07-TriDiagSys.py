# Solves system with tridiagonal matrix by LU factorization
from linsys import *

n = 4                                                       # order of system
a = [0]*(n+1)                                                # lower diagonal
b = [0]*(n+1)                                                 # main diagonal
c = [0]*(n+1)                                                # upper diagonal
d = [0]*(n+1)                                   # constant terms and solution

a[1] = 0; b[1] = 1; c[1] = 2; d[1] = 1
a[2] = 2; b[2] = 1; c[2] = 2; d[2] = 2
a[3] = 2; b[3] = 1; c[3] = 2; d[3] = 3
a[4] = 2; b[4] = 1; c[4] = 0; d[4] = 4       # Solution: -3.0, 2.0, 3.0, -2.0

TriDiagSys(a,b,c,d,n)                              # solve tridiagonal system

print("Solution:")
for i in range (1,n+1): print('{0:10.3f}'.format(d[i]),end="")
print()
