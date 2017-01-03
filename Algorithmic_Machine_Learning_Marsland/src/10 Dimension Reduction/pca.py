
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# An algorithm to compute PCA. Not as fast as the NumPy implementation
from pylab import *
from numpy import *

def pca(data,nRedDim=0,normalise=1):
    
    # Centre data
    m = mean(data,axis=0)
    data -= m

    # Covariance matrix
    C = cov(transpose(data))

    # Compute eigenvalues and sort into descending order
    evals,evecs = linalg.eig(C) 
    indices = argsort(evals)
    indices = indices[::-1]
    evecs = evecs[:,indices]
    evals = evals[indices]

    if nRedDim>0:
        evecs = evecs[:,:nRedDim]
    
    if normalise:
        for i in range(shape(evecs)[1]):
            evecs[:,i] / linalg.norm(evecs[:,i]) * sqrt(evals[i])

    # Produce the new data matrix
    x = dot(transpose(evecs),transpose(data))
    # Compute the original data again
    y=transpose(dot(evecs,x))+m
    return x,y,evals,evecs
    
