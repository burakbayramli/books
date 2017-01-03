
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Sampling-Importance-Resampling algorithm
from pylab import *
from numpy import *

def p(x):
    return 0.3*exp(-(x-0.3)**2) + 0.7* exp(-(x-2.)**2/0.3) 

def q(x):
    return 4.0

def sir(n):
    
    sample1 = zeros(n)
    w = zeros(n)
    sample2 = zeros(n)
    
    # Sample from q
    sample1 = random.rand(n)*4

    # Compute weights
    w = p(sample1)/q(sample1)
    w /= sum(w)

    # Sample from sample1 according to w
    cumw = zeros(len(w))
    cumw[0] = w[0]
    for i in range(1,len(w)):
        cumw[i] = cumw[i-1]+w[i]
    
    u = random.rand(n)
    
    index = 0
    for i in range(n):
        indices = where(u<cumw[i])
        sample2[index:index+size(indices)] = sample1[i]
        index += size(indices)
        u[indices]=2
    return sample2

x = arange(0,4,0.01)
x2 = arange(-0.5,4.5,0.1)
realdata = 0.3*exp(-(x-0.3)**2) + 0.7* exp(-(x-2.)**2/0.3) 
box = ones(len(x2))*0.8
box[:5] = 0
box[-5:] = 0
plot(x,realdata,'k',lw=6)
plot(x2,box,'k--',lw=6)

import time
t0=time.time()
samples = sir(10000)
t1=time.time()
print t1-t0
hist(samples,15,normed=1,fc='k')
xlabel('x',fontsize=24)
ylabel('p(x)',fontsize=24)
axis([-0.5,4.5,0,1])
show()
