
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Various dimensionality reductions running on the Iris dataset
from pylab import *
from numpy import *

iris = loadtxt('../3 MLP/iris_proc.data',delimiter=',')
iris[:,:4] = iris[:,:4]-iris[:,:4].mean(axis=0)
imax = concatenate((iris.max(axis=0)*ones((1,5)),iris.min(axis=0)*ones((1,5))),axis=0).max(axis=0)
iris[:,:4] = iris[:,:4]/imax[:4]
labels = iris[:,4]

order = range(shape(iris)[0])
random.shuffle(order)
iris = iris[order,:]
labels = labels[order,:]

w0 = where(labels==0)
w1 = where(labels==1)
w2 = where(labels==2)

#import lda
#newData,w = lda.lda(iris,labels,2)
#
#plot(iris[w0,0],iris[w0,1],'ok')
#plot(iris[w1,0],iris[w1,1],'^k')
#plot(iris[w2,0],iris[w2,1],'vk')
#axis([-1.5,1.8,-1.5,1.8])
#axis('off')
#figure(2)
#plot(newData[w0,0],newData[w0,1],'ok')
#plot(newData[w1,0],newData[w1,1],'^k')
#plot(newData[w2,0],newData[w2,1],'vk')
#axis([-1.5,1.8,-1.5,1.8])
#axis('off')
#
#import pca
#x,y,evals,evecs = pca.pca(iris,2)
#figure(3)
#plot(y[w0,0],y[w0,1],'ok')
#plot(y[w1,0],y[w1,1],'^k')
#plot(y[w2,0],y[w2,1],'vk')
#axis('off')

#import kernelpca
#newData = kernelpca.kernelpca(iris,'gaussian',2)
#figure(4)
#plot(newData[w0,0],newData[w0,1],'ok')
#plot(newData[w1,0],newData[w1,1],'^k')
#plot(newData[w2,0],newData[w2,1],'vk')
#axis('off')

#import factoranalysis
#newData = factoranalysis.factoranalysis(iris,2)
##print newData
##figure(5)
#plot(newData[w0,0],newData[w0,1],'ok')
#plot(newData[w1,0],newData[w1,1],'^k')
#plot(newData[w2,0],newData[w2,1],'vk')
#axis('off')

#import lle
#print shape(iris)
#a,b,newData = lle.lle(iris,2,12)
#print shape(newData)
#print newData[w0,:]
#print "---"
#print newData[w1,:]
#print "---"
#print newData[w2,:]
#
#plot(newData[w0,0],newData[w0,1],'ok')
#plot(newData[w1,0],newData[w1,1],'^k')
#plot(newData[w2,0],newData[w2,1],'vk')
#axis('off')

import isomap
print labels
newData,newLabels = isomap.isomap(iris,2,100)
print shape(newData)
print newLabels
w0 = where(newLabels==0)
w1 = where(newLabels==1)
w2 = where(newLabels==2)
plot(newData[w0,0],newData[w0,1],'ok')
plot(newData[w1,0],newData[w1,1],'^k')
plot(newData[w2,0],newData[w2,1],'vk')
axis('off')

show()
