
# Code from Chapter 4 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from pylab import *
from numpy import *

x = arange(-3,10,0.05)
y = 2.5 * exp(-(x)**2/9) + 3.2 * exp(-(x-0.5)**2/4) + random.normal(0.0, 1.0, len(x))
nParam = 2
A = zeros((len(x),nParam), float)
A[:,0] = exp(-(x)**2/9)
A[:,1] = exp(-(x*0.5)**2/4)
(p, residuals, rank, s) = linalg.lstsq(A,y)

print p
ion()
plot(x,y,'.')
plot(x,p[0]*A[:,0]+p[1]*A[:,1],'x')

show()
