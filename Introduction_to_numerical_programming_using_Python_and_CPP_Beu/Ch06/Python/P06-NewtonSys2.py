# Newton-Raphson method for systems of non-linear equations
from math import *
from roots import *

def Func(f, x, n):                                          # zero: (0, 0, 0)
   f[1] = x[3] - x[1]*x[2]*x[3]
   f[2] = x[1] + x[2] + x[3]
   f[3] = x[1]*x[2] + x[2]*x[3] + x[3]*x[1]

# main

n = 3
f = [0]*(n+1)
x = [0]*(n+1)
dx = [0]*(n+1)

x[1] = 1e0; x[2] = 2e0; x[3] = 3e0                    # initial approximation

(dx,ierr) = NewtonSys(Func,x,n)
Func(f,x,n)

print("Solution:")
print("           x           dx       f")
for i in range(1,n+1):
   print("{0:d}  {1:15.7e}  {2:7.0e}  {3:7.0}".format(i,x[i],dx[i],f[i]))
