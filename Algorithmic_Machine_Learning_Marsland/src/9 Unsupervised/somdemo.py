
# Code from Chapter 9 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A simple example of using the SOM on a 2D dataset showing the neighbourhood connections

from pylab import *
from numpy import *

import som
nNodesEdge = 8
data = (random.rand(2000,2)-0.5)*2

# Set up the network and decide on parameters
net = som.som(nNodesEdge,nNodesEdge,data,usePCA=0)
step = 0.2

figure(1)
plot(data[:,0],data[:,1],'.')
# Train the network for 0 iterations (to get the position of the nodes)
net.somtrain(data,0)
for i in range(net.x*net.y):
    neighbours = where(net.mapDist[i,:]<=step)

    t = zeros((shape(neighbours)[1]*2,shape(net.weights)[0]))
    t[::2,:] = tile(net.weights[:,i],(shape(neighbours)[1],1))
    t[1::2,:] = transpose(net.weights[:,neighbours[0][:]])
    plot(t[:,0],t[:,1],'g-')
axis('off')

figure(2)
plot(data[:,0],data[:,1],'.')
net.somtrain(data,5)
for i in range(net.x*net.y):
    neighbours = where(net.mapDist[i,:]<=step)

    t = zeros((shape(neighbours)[1]*2,shape(net.weights)[0]))
    t[::2,:] = tile(net.weights[:,i],(shape(neighbours)[1],1))
    t[1::2,:] = transpose(net.weights[:,neighbours[0][:]])
    plot(t[:,0],t[:,1],'g-')
axis([-1,1,-1,1])
axis('off')

net.somtrain(data,100)
figure(3)
plot(data[:,0],data[:,1],'.')
for i in range(net.x*net.y):
    neighbours = where(net.mapDist[i,:]<=step)
    #print neighbours
    #n = tile(net.weights[:,i],(shape(neighbours)[1],1))
    t = zeros((shape(neighbours)[1]*2,shape(net.weights)[0]))
    t[::2,:] = tile(net.weights[:,i],(shape(neighbours)[1],1))
    t[1::2,:] = transpose(net.weights[:,neighbours[0][:]])
    plot(t[:,0],t[:,1],'g-')
    
#net.somtrain(data,100)
#figure(4)
#plot(data[:,0],data[:,1],'.')
#for i in range(net.x*net.y):
#    neighbours = where(net.mapDist[i,:]<=step)
#    #print neighbours
#    #n = tile(net.weights[:,i],(shape(neighbours)[1],1))
#    t = zeros((shape(neighbours)[1]*2,shape(net.weights)[0]))
#    t[::2,:] = tile(net.weights[:,i],(shape(neighbours)[1],1))
#    t[1::2,:] = transpose(net.weights[:,neighbours[0][:]])
#    plot(t[:,0],t[:,1],'g-')
#    
#net.somtrain(data,100)
#figure(5)
#plot(data[:,0],data[:,1],'.')
#for i in range(net.x*net.y):
#    neighbours = where(net.mapDist[i,:]<=step)
#    #print neighbours
#    #n = tile(net.weights[:,i],(shape(neighbours)[1],1))
#    t = zeros((shape(neighbours)[1]*2,shape(net.weights)[0]))
#    t[::2,:] = tile(net.weights[:,i],(shape(neighbours)[1],1))
#    t[1::2,:] = transpose(net.weights[:,neighbours[0][:]])
#    plot(t[:,0],t[:,1],'g-')

show()
