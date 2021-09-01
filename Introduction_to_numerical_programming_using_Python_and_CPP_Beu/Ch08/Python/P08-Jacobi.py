# Eigenvalues and eigenvectors of symmetric matrices by the Jacobi method
from eigsys import *

n = 4
a = [[0]*(n+1) for i in range(n+1)]
x = [[0]*(n+1) for i in range(n+1)]
d = [0]*(n+1)

a[1][1] = 1                                                  # lower triangle
a[2][1] = 2; a[2][2] = 1
a[3][1] = 3; a[3][2] = 2; a[3][3] = 1
a[4][1] = 4; a[4][2] = 3; a[4][3] = 2; a[4][4] = 1
# Eigenvalues: -3.414214, -1.099019, -0.585786, 9.099020

for i in range(2,n+1):                              # complete upper triangle
   for j in range(1,i): a[j][i] = a[i][j]           # - not altered by Jacobi

Jacobi(a,x,d,n)
EigSort(x,d,n,1)                          # sort eigenvalues and eigenvectors

print("Eigenvalues:")
for i in range(1,n+1): print("{0:10.5f}".format(d[i]),end="")
print()

for i in range(2,n+1):                              # restore original matrix
   for j in range(1,i): a[i][j] = a[j][i]           # from upper triangle

err = 0e0                                                    # accuracy check
for i in range(1,n+1):
   for j in range(1,n+1):
      t = -x[i][j] * d[j]
      for k in range(1,n+1): t += a[i][k] * x[k][j]
      if (err < fabs(t)): err = fabs(t)           # err = max|a x - lambda x|

print("\nMaximum error = ","{0:8.2e}".format(err))
