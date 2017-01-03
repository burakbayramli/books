
# Code from Chapter 8 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from pylab import *
from numpy import *

def GMM():

    """ Fits two Gaussians to data using the EM algorithm """
    N = 100
    ion()
    
    y = 1.*zeros(N)
    # Set up data
    out1 = random.normal(6,1,N)
    out2 = random.normal(1,1,N)
    choice = random.rand(N)
    
    w = [choice>=0.5]
    y[w] = out1[w]	
    w = [choice<0.5]
    y[w] = out2[w]
    
    clf()
    hist(y,fc='0.5')
    
	# Now do some learning

	# Initialisation
    mu1 = y[random.randint(0,N-1,1)]
    mu2 = y[random.randint(0,N-1,1)]
    s1 = sum((y-mean(y))**2)/N
    s2 = s1
    pi = 0.5

	# EM loop
    count = 0
    gamma = 1.*zeros(N)
    nits = 20

    ll = 1.*zeros(nits)
	
    while count<nits:
        count = count + 1

    	# E-step
        for i in range(N):
            gamma[i] = pi*exp(-(y[i]-mu2)**2/s2)/((1-pi) * exp(-(y[i]-mu1)**2/s1) + pi* exp(-(y[i]-mu2)**2/s2))
        
    	# M-step
        mu1 = sum((1-gamma)*y)/sum(1-gamma)
        mu2 = sum(gamma*y)/sum(gamma)
        s1 = sum((1-gamma)*(y-mu1)**2)/sum(1-gamma)
        s2 = sum(gamma*(y-mu2)**2)/sum(gamma)
        pi = sum(gamma)/N
        	
        ll[count-1] = sum(log((1-pi)*exp(-(y[i]-mu1)**2/s1) + pi*exp(-(y[i]-mu2)**2/s2)))
    x = arange(-2,8.5,0.1)
    y = 35*pi*exp(-(x-mu1)**2/s1) + 35*(1-pi)*exp(-(x-mu2)**2/s2)
    plot(x,y,'k',linewidth=4)
    figure(), plot(ll,'ko-')
    show()
    
GMM()
