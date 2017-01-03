
# Code from Chapter 9 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from numpy import *

class kmeans:
    """The k-Means Algorithm implemented as a neural network"""
    def __init__(self,k,data,nEpochs=1000,eta=0.25):

        self.nData = shape(data)[0]
        self.nDim = shape(data)[1]
        self.k = k
        self.nEpochs = nEpochs
        self.weights = random.rand(self.nDim,self.k)
        self.eta = eta
        
    def kmeanstrain(self,data):
        # Preprocess data (won't work if (0,0,...0) is in data)
        normalisers = sqrt(sum(data**2,axis=1))*ones((1,shape(data)[0]))
        data = transpose(transpose(data)/normalisers)

        for i in range(self.nEpochs):
            for j in range(self.nData):
                activation = sum(self.weights*transpose(data[j:j+1,:]),axis=0)
                winner = argmax(activation)
                self.weights[:,winner] += self.eta * data[j,:] - self.weights[:,winner]            
            
    def kmeansfwd(self,data):
        best = zeros(shape(data)[0])
        for i in range(shape(data)[0]):
            activation = sum(self.weights*transpose(data[i:i+1,:]),axis=0)
            best[i] = argmax(activation)
        return best
    
