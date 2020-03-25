""" Snoisy.py """
import numpy as np
def Snoisy(X):   #takes a matrix
    n = X.shape[1]
    N = X.shape[0]
    xorg = np.hstack((np.ones((1,n//2)), np.zeros((1,n//2)))) #true binary vector
    theta = 0.4 # probability to flip the input
    s = np.zeros(N)  #storing the number of bits unequal to the true vector
    for i in range(0,N):
       flip = (np.random.uniform(size=(n)) < theta).astype(int) # determine which bits to flip
       ind = flip>0
       X[i][ind] = 1-X[i][ind]
       s[i] = (X[i] != xorg).sum()
    return s