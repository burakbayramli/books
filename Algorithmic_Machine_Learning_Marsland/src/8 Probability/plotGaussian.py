
# Code from Chapter 8 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Plots a 1D Gaussian function
from pylab import *
from numpy import *

gaussian = lambda x: 1/(sqrt(2*pi)*1.5)*exp(-(x-0)**2/(2*(1.5**2)))
x = arange(-5,5,0.01)
y = gaussian(x)
ion()
plot(x,y,'k',linewidth=3)
xlabel('x')
ylabel('y(x)')
axis([-5,5,0,0.3])
title('Gaussian Function (mean 0, standard deviation 1.5)')
show()
