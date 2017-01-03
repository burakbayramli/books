
# Code from Chapter 4 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from numpy import *
import pcn
import kmeans

class rbf:
    """ The Radial Basis Function network
    Parameters are number of RBFs, and their width, how to train the network 
    (pseudo-inverse or kmeans) and whether the RBFs are normalised"""

    def __init__(self,inputs,targets,nRBF,sigma=0,usekmeans=0,normalise=0):
        self.nin = shape(inputs)[1]
        self.nout = shape(targets)[1]
        self.ndata = shape(inputs)[0]
        self.nRBF = nRBF
        self.usekmeans = usekmeans
        self.normalise = normalise
        
        if usekmeans:
            self.kmeansnet = kmeans.kmeans(self.nRBF,inputs)
            
        self.hidden = zeros((self.ndata,self.nRBF+1))
        
        if sigma==0:
            # Set width of Gaussians
            d = (inputs.max(axis=0)-inputs.min(axis=0)).max()
            self.sigma = d/sqrt(2*nRBF)  
        else:
            self.sigma = sigma
                
        self.perceptron = pcn.pcn(self.hidden[:,:-1],targets)
        
        # Initialise network
        self.weights1 = zeros((self.nin,self.nRBF))
        
    def rbftrain(self,inputs,targets,eta=0.25,niterations=100):
                
        if self.usekmeans==0:
            # Version 1: set RBFs to be datapoints
            indices = range(self.ndata)
            random.shuffle(indices)
            for i in range(self.nRBF):
                self.weights1[:,i] = inputs[indices[i],:]
        else:
            # Version 2: use k-means
            self.weights1 = transpose(self.kmeansnet.kmeanstrain(inputs))

        for i in range(self.nRBF):
            self.hidden[:,i] = exp(-sum((inputs - ones((1,self.nin))*self.weights1[:,i])**2,axis=1)/(2*self.sigma**2))
        if self.normalise:
            self.hidden[:,:-1] /= transpose(ones((1,shape(self.hidden)[0]))*self.hidden[:,:-1].sum(axis=1))
        
        # Call Perceptron without bias node (since it adds its own)
        self.perceptron.pcntrain(self.hidden[:,:-1],targets,eta,niterations)
        
    def rbffwd(self,inputs):

        hidden = zeros((shape(inputs)[0],self.nRBF+1))

        for i in range(self.nRBF):
            hidden[:,i] = exp(-sum((inputs - ones((1,self.nin))*self.weights1[:,i])**2,axis=1)/(2*self.sigma**2))

        if self.normalise:
            hidden[:,:-1] /= transpose(ones((1,shape(hidden)[0]))*hidden[:,:-1].sum(axis=1))
        
        # Add the bias
        hidden[:,-1] = -1

        outputs = self.perceptron.pcnfwd(hidden)
        return outputs
    
    def confmat(self,inputs,targets):
        """Confusion matrix"""

        outputs = self.rbffwd(inputs)
        nClasses = shape(targets)[1]

        if nClasses==1:
            nClasses = 2
            outputs = where(outputs>0,1,0)
        else:
            # 1-of-N encoding
            outputs = argmax(outputs,1)
            targets = argmax(targets,1)

        cm = zeros((nClasses,nClasses))
        for i in range(nClasses):
            for j in range(nClasses):
                cm[i,j] = sum(where(outputs==i,1,0)*where(targets==j,1,0))

        print cm
        print trace(cm)/sum(cm)
