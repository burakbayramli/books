""" svdexample.py """
from numpy import diag, zeros,vstack
from numpy.random import rand, seed
from numpy.linalg import svd, pinv
seed(12345)
n = 5
p = 8
X = rand(n,p)
y = rand(n,1)
U,S,VT = svd(X)  
SI  = diag(1/S)
# compute pseudo inverse
pseudo_inv = VT.T @ vstack((SI, zeros((p-n,n)))) @ U.T
b = pseudo_inv @ y
#b = pinv(X) @ y   #remove comment to use the built-in pseudo inverse
print(X @ b - y)