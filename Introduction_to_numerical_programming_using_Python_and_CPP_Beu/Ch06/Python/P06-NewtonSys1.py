# Intersection of circle and parabola
from math import *
from roots import *

def Func(f, x, n):                   # zeros: (-2.295, 2.267), (2.583, 3.673)
   f[1] = pow(x[1]-1,2) + x[2]*x[2] - 16e0
   f[2] = x[1]*x[1] - x[2] - 3e0

# main

n = 2
f = [0]*(n+1)
x = [0]*(n+1)
dx = [0]*(n+1)

x[1] = -5e0; x[2] = 5e0        # 1st initial approximation -> (-2.295, 2.267)
(dx,ierr) = NewtonSys(Func,x,n)
Func(f,x,n)

print("Solution 1:")
print("           x           dx       f")
for i in range(1,n+1):
   print("{0:d}  {1:15.7e}  {2:7.0e}  {3:7.0}".format(i,x[i],dx[i],f[i]))

x[1] = 5e0; x[2] = 5e0          # 2nd initial approximation -> (2.583, 3.673)
(dx,ierr) = NewtonSys(Func,x,n)
Func(f,x,n)

print("\nSolution 2:")
print("           x           dx       f")
for i in range(1,n+1):
   print("{0:d}  {1:15.7e}  {2:7.0e}  {3:7.0}".format(i,x[i],dx[i],f[i]))
