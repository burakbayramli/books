## example9_14
from householder import *
from eigenvals3 import *
from inversePower3 import *
from numarray import array,zeros,Float64,matrixmultiply

N = 3   # Number of eigenvalues requested
a = array([[ 11.0, 2.0,  3.0,  1.0,  4.0],  \
           [  2.0, 9.0,  3.0,  5.0,  2.0],  \
           [  3.0, 3.0, 15.0,  4.0,  3.0],  \
           [  1.0, 5.0,  4.0, 12.0,  4.0],  \
           [  4.0, 2.0,  3.0,  4.0, 17.0]])
xx = zeros((len(a),N),type=Float64)
d,c = householder(a)             # Tridiagonalize [A]
p = computeP(a)                  # Compute transformation matrix
lambdas = eigenvals3(d,c,N)      # Compute eigenvalues
for i in range(N):            
    s = lambdas[i]*1.0000001     # Shift very close to eigenvalue
    lam,x = inversePower3(d,c,s) # Compute eigenvector [x]
    xx[:,i] = x                  # Place [x] in array [xx]
xx = matrixmultiply(p,xx)        # Recover eigenvectors of [A]
print "Eigenvalues:\n",lambdas
print "\nEigenvectors:\n",xx
raw_input("Press return to exit")
    
