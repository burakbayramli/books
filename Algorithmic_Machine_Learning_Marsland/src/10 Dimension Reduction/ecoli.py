
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Simple example of LDA, PCA, and kernel PCA, on the Wine and e-coli datasets
from pylab import *
from numpy import *

wine = loadtxt('../9 Unsupervised/wine.data',delimiter=',')

labels = wine[:,0]
data = wine[:,1:]
data -= mean(data,axis=0)
data /= data.max(axis=0)

#ecoli = loadtxt('../9 Unsupervised/shortecoli.data')
#labels = ecoli[:,7:]
#data = ecoli[:,:7]
#data -= mean(data,axis=0)
#data /= data.max(axis=0)

order = range(shape(data)[0])
random.shuffle(order)
data = data[order]
w0 = where(labels==1)
w1 = where(labels==2)
w2 = where(labels==3)

import lda
newData,w = lda.lda(data,labels,2)

plot(data[w0,0],data[w0,1],'ok')
plot(data[w1,0],data[w1,1],'^k')
plot(data[w2,0],data[w2,1],'vk')
axis([-1.5,1.8,-1.5,1.8])
axis('off')
figure(2)
plot(newData[w0,0],newData[w0,1],'ok')
plot(newData[w1,0],newData[w1,1],'^k')
plot(newData[w2,0],newData[w2,1],'vk')
axis([-1.5,1.8,-1.5,1.8])
axis('off')

import pca
x,y,evals,evecs = pca.pca(data,2)
figure(3)
plot(y[w0,0],y[w0,1],'ok')
plot(y[w1,0],y[w1,1],'^k')
plot(y[w2,0],y[w2,1],'vk')
axis('off')

import kernelpca
newData = kernelpca.kernelpca(data,'gaussian',2)
figure(4)
plot(newData[w0,0],newData[w0,1],'ok')
plot(newData[w1,0],newData[w1,1],'^k')
plot(newData[w2,0],newData[w2,1],'vk')
axis('off')

show()
