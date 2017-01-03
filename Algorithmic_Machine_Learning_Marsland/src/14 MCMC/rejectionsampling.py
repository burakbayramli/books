
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The basic rejection sampling algorithm

from pylab import *
from numpy import *

def qsample():
    return random.rand()*4.

def p(x):
    return 0.3*exp(-(x-0.3)**2) + 0.7* exp(-(x-2.)**2/0.3) 

def rejection(nsamples):
    
    M = 0.72#0.8
    samples = zeros(nsamples,dtype=float)
    count = 0
    for i in range(nsamples):
        accept = False
        while not accept:
            x = qsample()
            u = random.rand()*M
            if u<p(x):
                accept = True
                samples[i] = x
            else: 
                count += 1
    print count   
    return samples

x = arange(0,4,0.01)
x2 = arange(-0.5,4.5,0.1)
realdata = 0.3*exp(-(x-0.3)**2) + 0.7* exp(-(x-2.)**2/0.3) 
box = ones(len(x2))*0.75#0.8
box[:5] = 0
box[-5:] = 0
plot(x,realdata,'k',lw=6)
plot(x2,box,'k--',lw=6)

import time
t0=time.time()
samples = rejection(10000)
t1=time.time()
print "Time ",t1-t0

hist(samples,15,normed=1,fc='k')
xlabel('x',fontsize=24)
ylabel('p(x)',fontsize=24)
axis([-0.5,4.5,0,1])
show()
