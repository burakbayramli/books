
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Box-Muller algorithm for constructing pseudo-random Gaussian-distributed numbers

from pylab import *
from numpy import *

def boxmuller(n):
    
    x = zeros((n,2))
    y = zeros((n,2))
    
    for i in range(n):
        x[i,:] = array([2,2])
        x2 = x[i,0]*x[i,0]+x[i,1]*x[i,1]
        while (x2)>1:
            x[i,:] = random.rand(2)*2-1
            x2 = x[i,0]*x[i,0]+x[i,1]*x[i,1]

        y[i,:] = x[i,:] * sqrt((-2*log(x2))/x2)
    
    y = reshape(y,2*n,1)
    return y

y = boxmuller(1000)
hist(y,normed=1,fc='k')
x = arange(-4,4,0.1)
plot(x,1/sqrt(2*pi)*exp(-0.5*x**2),'k',lw=6)
xlabel('x',fontsize=24)
ylabel('p(x)',fontsize=24)
show()
    
    
