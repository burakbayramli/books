# Newton-Raphson method for systems of non-linear equations
from math import *
from roots import *

def Func(f, x, n):                                    # zeros: (1, 2), (2, 1)
   f[1] = pow(x[1],3) * x[2] + x[1] * pow(x[2],3) - 10e0
   f[2] = pow(x[1],2) * x[2] + x[1] * pow(x[2],2) - 6e0

# main

n = 2
f = [0]*(n+1); x = [0]*(n+1); dx = [0]*(n+1)

x[1] = 0e0; x[2] = 0e0                                # initial approximation
(dx,ierr) = NewtonSys(Func,x,n)
Func(f,x,n)

print("\nSolution:")
print("           x           dx        f")
for i in range(1,n+1):
   print("{0:d}  {1:15.7e}  {2:7.0e}  {3:7.0}".format(i,x[i],dx[i],f[i]))
