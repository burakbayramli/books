# Check matrix identity: (A*B)^(-1) = B^(-1)*A^(-1)
from random import *
from linsys import *
from matutil import *

n = 100
a = [[0]*(n+1) for i in range(n+1)]
b = [[0]*(n+1) for i in range(n+1)]
c = [[0]*(n+1) for i in range(n+1)]
d = [[0]*(n+1) for i in range(n+1)]

for i in range(1,n+1):                   # generate matrices a and b randomly
   for j in range(1,n+1):
      a[i][j] = random()
      b[i][j] = random()

MatProd(a,b,c,n,n,n)                                              # c = a * b
det = GaussJordan(c,d,n,0)  # det = MatInv(c,n)              # c = (a b)^(-1)

det = GaussJordan(a,d,n,0)  # det = MatInv(a,n)                 # a -> a^(-1)
det = GaussJordan(b,d,n,0)  # det = MatInv(b,n)                 # b -> b^(-1)
MatProd(b,a,d,n,n,n)                                      # d = b^(-1) a^(-1)

MatDiff(c,d,d,n,n)                            # d = (a b)^(-1) - b^(-1)a^(-1)
print("(A B)^(-1) - B^(-1)A^(-1) (sample):\n")
MatPrint(d,5,5)                               # print sample of "zero" matrix

err = MatNorm(d,n,n)                                      # max(abs(d[i][j]))
print("\nMaximum error = ","{0:8.2e}".format(err))
