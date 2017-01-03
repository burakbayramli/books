
# Code from Chapter 3 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The sinewave regression example

from pylab import *
from numpy import *

# Set up the data
x = ones((1,40))*linspace(0,1,40)
t = sin(2*pi*x) + cos(4*pi*x) + random.randn(40)*0.2
x = (x-0.5)*2
x = transpose(x)
t = transpose(t)

# Split into training, testing, and validation sets
train = x[0::2,:]
test = x[1::4,:]
valid = x[3::4,:]
traintarget = t[0::2,:]
testtarget = t[1::4,:]
validtarget = t[3::4,:]

# Plot the data
plot(x,t,'o')
xlabel('x')
ylabel('t')

# Perform basic training with a small MLP
import mlp
net = mlp.mlp(train,traintarget,3,outtype='linear')
net.mlptrain(train,traintarget,0.25,101)

# Use early stopping
net.earlystopping(train,traintarget,valid,validtarget,0.25)

# Test out different sizes of network
#count = 0
#out = zeros((10,7))
#for nnodes in [1,2,3,5,10,25,50]:
#    for i in range(10):
#        net = mlp.mlp(train,traintarget,nnodes,outtype='linear')
#        out[i,count] = net.earlystopping(train,traintarget,valid,validtarget,0.25)
#    count += 1
#    
#test = concatenate((test,-ones((shape(test)[0],1))),axis=1)
#outputs = net.mlpfwd(test)
#print 0.5*sum((outputs-testtarget)**2)
#
#print out
#print out.mean(axis=0)
#print out.var(axis=0)
#print out.max(axis=0)
#print out.min(axis=0)

show()