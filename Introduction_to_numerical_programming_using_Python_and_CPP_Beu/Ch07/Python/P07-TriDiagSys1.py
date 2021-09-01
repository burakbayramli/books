# Solves system with tridiagonal matrix by LU factorization
from random import *
from linsys import *

n = 100                                                     # order of system
a = [0]*(n+1); a0 = [0]*(n+1)                                # lower diagonal
b = [0]*(n+1); b0 = [0]*(n+1)                                 # main diagonal
c = [0]*(n+1); c0 = [0]*(n+1)                                # upper diagonal
d = [0]*(n+1); x  = [0]*(n+1)                   # constant terms and solution

for i in range(1,n+1):                                      # generate system
   if (i > 1): a[i] = a0[i] = random()                          # a[i], i=2,n
   if (i < n): c[i] = c0[i] = random()                        # c[i], i=1,n-1
   b[i] = b0[i] = random()
   d[i] = x[i]  = random()

TriDiagSys(a,b,c,x,n)                              # solve tridiagonal system

err = 0e0                                               # check max(Ax-d) = 0
for i in range(1,n+1):
   erri = b0[i]*x[i] - d[i]                             # element i of (Ax-d)
   if (i > 1): erri += a0[i]*x[i-1]
   if (i < n): erri += c0[i]*x[i+1]
   err = max(err,fabs(erri))
print("max(Ax-d) = {0:.1e}".format(err))
