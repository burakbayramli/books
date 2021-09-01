# Solves multiple linear systems by LU factorization
from random import *
from linsys import *
from matutil import *

n = 5                                                       # order of system
a  = [[0]*(n+1) for i in range(n+1)]                          # system matrix
a0 = [[0]*(n+1) for i in range(n+1)]                          # backup matrix
b = [0]*(n+1)                                                # constant terms
x = [0]*(n+1)                                                      # solution
ipivot = [0]*(n+1)                                                   # pivots

for i in range(1,n+1):                                        # random matrix
   for j in range(1,n+1): a[i][j] = a0[i][j] = random()
print("A:")
MatPrint(a,n,n)

det = LUFactor(a,ipivot,n)                            # LU decomposition of a
print("LU decomposition:")
MatPrint(a,n,n)

for k in range(1,n+1):                           # loop over constant vectors
   for i in range(1,n+1): b[i] = x[i] = random()      # random constant terms
   print("b:")
   VecPrint(b,n)

   LUSystem(a,ipivot,x,n)                               # solve linear system

   err = 0e0                                            # check max(Ax-b) = 0
   for i in range(1,n+1):
      erri = -b[i]
      for j in range (1,n+1): erri += a0[i][j] * x[j]   # element i of (Ax-b)
      err = max(err,fabs(erri))
   print("   max(Ax-b) = {0:.1e}".format(err))
