
# Code from Chapter 4 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from numpy import *

iris = loadtxt('../3 MLP/iris_proc.data',delimiter=',')
iris[:,:4] = iris[:,:4]-iris[:,:4].mean(axis=0)
imax = concatenate((iris.max(axis=0)*ones((1,5)),iris.min(axis=0)*ones((1,5))),axis=0).max(axis=0)
iris[:,:4] = iris[:,:4]/imax[:4]
#print iris[0:5,:]

#target = zeros((shape(iris)[0],2));
#indices = where(iris[:,4]==0) 
#target[indices,0] = 1
#indices = where(iris[:,4]==1)
#target[indices,1] = 1
#indices = where(iris[:,4]==2)
#target[indices,0] = 1
#target[indices,1] = 1

target = zeros((shape(iris)[0],3));
indices = where(iris[:,4]==0) 
target[indices,0] = 1
indices = where(iris[:,4]==1)
target[indices,1] = 1
indices = where(iris[:,4]==2)
target[indices,2] = 1


order = range(shape(iris)[0])
random.shuffle(order)
iris = iris[order,:]
target = target[order,:]

train = iris[::2,0:4]
traint = target[::2]
valid = iris[1::4,0:4]
validt = target[1::4]
test = iris[3::4,0:4]
testt = target[3::4]

#print train.max(axis=0), train.min(axis=0)

import rbf
net = rbf.rbf(train,traint,5,1,1)

net.rbftrain(train,traint,0.25,2000)
#net.confmat(train,traint)
net.confmat(test,testt)
