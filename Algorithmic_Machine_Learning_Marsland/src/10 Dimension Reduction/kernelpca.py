
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Kernel PCA algorithm

from pylab import *
from numpy import *

def kernelmatrix(data,kernel,param=array([3,2])):
    
    if kernel=='linear':
        return dot(data,transpose(data))
    elif kernel=='gaussian':
        K = zeros((shape(data)[0],shape(data)[0]))
        for i in range(shape(data)[0]):
            for j in range(i+1,shape(data)[0]):
                K[i,j] = sum((data[i,:]-data[j,:])**2)
                K[j,i] = K[i,j]
        return exp(-K**2/(2*param[0]**2))
    elif kernel=='polynomial':
        return (dot(data,transpose(data))+param[0])**param[1]
    
def kernelpca(data,kernel,redDim):
    
    nData = shape(data)[0]
    nDim = shape(data)[1]
    
    K = kernelmatrix(data,kernel)
    
    # Compute the transformed data
    D = sum(K,axis=0)/nData
    E = sum(D)/nData
    J = ones((nData,1))*D
    K = K - J - transpose(J) + E*ones((nData,nData))
    
    # Perform the dimensionality reduction
    evals,evecs = linalg.eig(K) 
    indices = argsort(evals)
    indices = indices[::-1]
    evecs = evecs[:,indices[:redDim]]
    evals = evals[indices[:redDim]]
    
    sqrtE = zeros((len(evals),len(evals)))
    for i in range(len(evals)):
        sqrtE[i,i] = sqrt(evals[i])
       
    #print shape(sqrtE), shape(data)
    newData = transpose(dot(sqrtE,transpose(evecs)))
    
    return newData

#data = array([[0.1,0.1],[0.2,0.2],[0.3,0.3],[0.35,0.3],[0.4,0.4],[0.6,0.4],[0.7,0.45],[0.75,0.4],[0.8,0.35]])
#newData = kernelpca(data,'gaussian',2)
#plot(data[:,0],data[:,1],'o',newData[:,0],newData[:,0],'.')
#show()
