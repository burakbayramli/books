# Check matrix identity det(A*B) = det(A)*det(B)
from random import *
from linsys import *
from matutil import *

n = 100
a = [[0]*(n+1) for i in range(n+1)]
b = [[0]*(n+1) for i in range(n+1)]
c = [[0]*(n+1) for i in range(n+1)]

for i in range(1,n+1):                   # generate matrices a and b randomly
   for j in range(1,n+1):
      a[i][j] = random()
      b[i][j] = random()

MatProd(a,b,c,n,n,n)                                              # C = A * B
detA  = Gauss(a,b,n,0)                       # m=0: performs only elimination
detB  = Gauss(b,b,n,0)                       # and calculates the determinant
detAB = Gauss(c,b,n,0)

print("det(A)  = {0:9.2e}".format(detA))
print("det(B)  = {0:9.2e}".format(detB))
print("det(AB) = {0:9.2e}".format(detAB))
print("Error   = {0:9.2e}".format(1e0 - detA*detB/detAB))
