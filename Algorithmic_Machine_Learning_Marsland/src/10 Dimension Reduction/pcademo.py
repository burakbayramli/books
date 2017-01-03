
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A simple example of PCA
from pylab import *
from numpy import *

import pca

x = random.normal(5,.5,1000)
y = random.normal(3,1,1000)
a = x*cos(pi/4) + y*sin(pi/4)
b = -x*sin(pi/4) + y*cos(pi/4)

plot(a,b,'.')
xlabel('x')
ylabel('y')
title('Original dataset')
data = zeros((1000,2))
data[:,0] = a
data[:,1] = b

x,y,evals,evecs = pca.pca(data,1)
print y
figure()
plot(y[:,0],y[:,1],'.')
xlabel('x')
ylabel('y')
title('Reconstructed data after PCA')
show()
