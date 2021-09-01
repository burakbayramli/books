# Solves linear system for Daubechies D4 wavelet by Gauss-Seidel iteration
from math import *
from linsys import *
from matutil import *

n = 8
a = [[0]*(n+1) for i in range(n+1)]                      # coefficient matrix
b = [0]*(n+1)                                                # constant terms
x = [0]*(n+1)                                                      # solution

sqrt3 = sqrt(3e0)
f = 1e0/(4*sqrt(2e0))
c0 = (1e0 + sqrt3) * f                   # Daubechies D4 wavelet coefficients
c1 = (3e0 + sqrt3) * f
c2 = (3e0 - sqrt3) * f
c3 = (1e0 - sqrt3) * f

for i in range(1,n+1):
   for j in range(1,n+1): a[i][j] = 0e0

a[1][1] = c0; a[1][2] = c1; a[1][3] = c2; a[1][4] = c3; b[1] =  143e0
a[2][1] = c3; a[2][2] =-c2; a[2][3] = c1; a[2][4] =-c0; b[2] = 1543e0
a[3][3] = c0; a[3][4] = c1; a[3][5] = c2; a[3][6] = c3; b[3] = 2200e0
a[4][3] = c3; a[4][4] =-c2; a[4][5] = c1; a[4][6] =-c0; b[4] = -403e0
a[5][5] = c0; a[5][6] = c1; a[5][7] = c2; a[5][8] = c3; b[5] =  591e0
a[6][5] = c3; a[6][6] =-c2; a[6][7] = c1; a[6][8] =-c0; b[6] =   50e0
a[7][1] = c2; a[7][2] = c3; a[7][7] = c0; a[7][8] = c1; b[7] =   68e0
a[8][1] = c1; a[8][2] =-c0; a[8][7] = c3; a[8][8] =-c2; b[8] =  -58e0

for i in range(1,n+1): x[i] = 1e0         # initial approximation of solution

err = GaussSeidel(a,b,x,n,0)

print("Solution:")
for i in range (1,n+1): print('{0:10.3f}'.format(x[i]),end="")
print()

err = 0e0                                                    # check solution
for i in range(1,n+1):
   erri = -b[i]
   for j in range(1,n+1): erri += a[i][j] * x[j]        # element i of (Ax-b)
   err = max(err,fabs(erri))                                # err = max(Ax-b)
print("\nMaximum error = ","{0:8.2e}".format(err))
