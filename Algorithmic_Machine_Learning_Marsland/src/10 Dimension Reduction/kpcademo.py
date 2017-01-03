
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Demonstration of PCA and kernel PCA on the circular dataset
from pylab import *
from numpy import *

import pca
import kernelpca

data = zeros((150,2))

theta = random.normal(0,pi,50)
r = random.normal(0,0.1,50)
data[0:50,0] = r*cos(theta)
data[0:50,1] = r*sin(theta)

theta = random.normal(0,pi,50)
r = random.normal(2,0.1,50)
data[50:100,0] = r*cos(theta)
data[50:100,1] = r*sin(theta)

theta = random.normal(0,pi,50)
r = random.normal(5,0.1,50)
data[100:150,0] = r*cos(theta)
data[100:150,1] = r*sin(theta)

figure()
plot(data[:50,0],data[:50,1],'ok')
plot(data[50:100,0],data[50:100,1],'^k')
plot(data[100:150,0],data[100:150,1],'vk')
title('Original dataset')

x,y,evals,evecs = pca.pca(data,2)
figure()
plot(x[:50,0],x[:50,1],'ok')
plot(x[50:100,0],x[50:100,1],'^k')
plot(x[100:150,0],x[100:150,1],'vk')
title('Reconstructed points after PCA')

figure()
y = kernelpca.kernelpca(data,'gaussian',2)
plot(y[:50,0],y[:50,1],'ok')
plot(y[50:100,0],y[50:100,1],'^k')
plot(y[100:150,0],y[100:150,1],'vk')
title('Reconstructed points after kernel PCA')

show()
