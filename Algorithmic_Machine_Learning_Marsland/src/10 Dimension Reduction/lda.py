
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The LDA algorithm

from pylab import *
from numpy import *
from scipy import linalg as la

def lda(data,labels,redDim):

    # Centre data
    data -= data.mean(axis=0)
    nData = shape(data)[0]
    nDim = shape(data)[1]
    
    Sw = zeros((nDim,nDim))
    Sb = zeros((nDim,nDim))
    
    C = cov(transpose(data))
    
    # Loop over classes
    classes = unique(labels)
    for i in range(len(classes)):
        # Find relevant datapoints
        indices = squeeze(where(labels==classes[i]))
        d = squeeze(data[indices,:])
        classcov = cov(transpose(d))
        Sw += float(shape(indices)[0])/nData * classcov
        
    Sb = C - Sw
    # Now solve for W
    # Compute eigenvalues, eigenvectors and sort into order
    #evals,evecs = linalg.eig(dot(linalg.pinv(Sw),sqrt(Sb)))
    evals,evecs = la.eig(Sw,Sb)
    indices = argsort(evals)
    indices = indices[::-1]
    evecs = evecs[:,indices]
    evals = evals[indices]
    w = evecs[:,:redDim]
    #print evals, w
    
    newData = dot(data,w)
    return newData,w

#data = array([[0.1,0.1],[0.2,0.2],[0.3,0.3],[0.35,0.3],[0.4,0.4],[0.6,0.4],[0.7,0.45],[0.75,0.4],[0.8,0.35]])
#labels = array([0,0,0,0,0,1,1,1,1])
#newData,w = lda(data,labels,2)
#print w
#plot(data[:,0],data[:,1],'o',newData[:,0],newData[:,0],'.')
#show()
