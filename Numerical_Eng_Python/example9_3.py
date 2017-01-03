## example9_3
from numarray import array,zeros,Float64
from stdForm import *
from jacobi import *
from sortJacobi import *

n = 10
a = zeros((n,n),type=Float64)
b = zeros((n,n),type=Float64)
for i in range(n):
    a[i,i] = 6.0
    b[i,i] = 2.0
a[0,0] = 5.0
a[n-1,n-1] = 7.0
for i in range(n-1):
    a[i,i+1] = -4.0
    a[i+1,i] = -4.0
    b[i,i+1] = -1.0
    b[i+1,i] = -1.0
for i in range(n-2):
    a[i,i+2] = 1.0
    a[i+2,i] = 1.0

h,t = stdForm(a,b)          # Convert to std. form
lam,z = jacobi(h)           # Solve by Jacobi mthd.
x = matrixmultiply(t,z)     # Eigenvectors of orig. prob.
for i in range(n):          # Normalize eigenvectors
    xMag = sqrt(dot(x[:,i],x[:,i]))
    x[:,i] = x[:,i]/xMag
sortJacobi(lam,x)       # Arrange in ascending order
print "Eigenvalues:\n",lam[0:3]
print "\nEigenvectors:\n",x[:,0:3]  
raw_input("\n Press return to exit")
