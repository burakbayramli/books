
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The basic importance sampling algorithm
from pylab import *
from numpy import *

def qsample():
    return random.rand()*4.

def p(x):
    return 0.3*exp(-(x-0.3)**2) + 0.7* exp(-(x-2.)**2/0.3) 

def q(x):
    return 4.0

def importance(nsamples):
    
    samples = zeros(nsamples,dtype=float)
    w = zeros(nsamples,dtype=float)
    
    for i in range(nsamples):
            samples[i] = qsample()
            w[i] = p(samples[i])/q(samples[i])
                
    return samples, w

x = arange(0,4,0.01)
x2 = arange(-0.5,4.5,0.1)
realdata = 0.3*exp(-(x-0.3)**2) + 0.7* exp(-(x-2.)**2/0.3) 
box = ones(len(x2))*0.8
box[:5] = 0
box[-5:] = 0
plot(x,realdata,'k',lw=6)
plot(x2,box,'k--',lw=6)

samples,w = importance(5000)
hist(samples,normed=1,fc='k')
#xlabel('x',fontsize=24)
#ylabel('p(x)',fontsize=24)
show()
