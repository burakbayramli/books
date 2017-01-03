
# Code from Chapter 8 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from pylab import *
from numpy import *

# A k-Nearest Neighbour smoother, with three different kernels
# Example is the Ruapehu dataset
def knnSmoother(k,data,testpoints,kernel):

    outputs = zeros(len(testpoints))
    
    for i in range(len(testpoints)):
        distances = (data[:,0]-testpoints[i])
        if kernel=='NN':
            indices = argsort(distances**2,axis=0)
            outputs[i] = 1./k * sum(data[indices[:k],1])
        elif kernel=='Epan':
            Klambda = 0.75*(1 - distances**2/k**2)
            where = (abs(distances)<k)
            outputs[i] = sum(Klambda*where*data[:,1])/sum(Klambda*where)
        elif kernel=='Tricube':
            Klambda = (1 - abs((distances/k)**3)**3)
            where = (abs(distances)<k)
            outputs[i] = sum(Klambda*where*data[:,1])/sum(Klambda*where)
        else:
            print('Unknown kernel')
    return outputs

data = loadtxt('ruapehu.dat') 
# Data is time of start and stop
# Turn into repose and duration 
t1 = data[:,0:1] 
t2 = data[:,1:2] 
repose = t1[1:len(t1),:] -t2[0:len(t2)-1,:] 
duration = t2[1:len(t2),:] -t1[1:len(t1),:]
order = argsort(repose,axis=0)
repose = repose[order]
duration = duration[order]
data = squeeze(concatenate((repose,duration),axis=1))
testpoints = 12.0*arange(1000)/1000
outputs5 = knnSmoother(5,data,testpoints,'NN')
outputs10 = knnSmoother(10,data,testpoints,'NN')

plot(data[:,0],data[:,1],'ko',testpoints,outputs5,'k-',linewidth=3)
plot(testpoints,outputs10,'k--',linewidth=3)
legend(('Data','NN, k=5','NN, k=10'))
xlabel('Repose (years)')
ylabel('Duration (years)')

figure(2)
outputs5 = knnSmoother(2,data,testpoints,'Epan')
outputs10 = knnSmoother(4,data,testpoints,'Epan')

plot(data[:,0],data[:,1],'ko',testpoints,outputs5,'k-',linewidth=3)
plot(testpoints,outputs10,'k--',linewidth=3)
legend(('Data','Epanechnikov, lambda=2','Epanechnikov, lambda=4'))
xlabel('Repose (years)')
ylabel('Duration (years)')

figure(3)
outputs5 = knnSmoother(2,data,testpoints,'Tricube')
outputs10 = knnSmoother(4,data,testpoints,'Tricube')

plot(data[:,0],data[:,1],'ko',testpoints,outputs5,'k-',linewidth=3)
plot(testpoints,outputs10,'k--',linewidth=3)
legend(('Data','Tricube, lambda=2','Tricube, lambda=4'))
xlabel('Repose (years)')
ylabel('Duration (years)')


show()
