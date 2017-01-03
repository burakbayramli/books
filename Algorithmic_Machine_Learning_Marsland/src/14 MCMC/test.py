
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Metropolis-Hastings algorithm
import numpy as np
import matplotlib.pyplot as plt

def p(x):
    mu1 = 3; mu2 = 10
    v1 = 10; v2 = 3
    return 0.3*np.exp(-(x-mu1)**2/v1) + 0.7* np.exp(-(x-mu2)**2/v2)

def q(x):
    mu = 5; sigma = 10
    return np.exp(-(x-mu)**2/(sigma**2))

stepsize = 0.5
x = np.arange(-10,20,stepsize)
px = np.zeros(x.shape)
for i in range(len(x)): px[i] = p(x[i])
N = 5000

# independence chain
u = np.random.rand(N)
mu = 5
sigma = 10
y = np.zeros(N)
y[0] = np.random.normal(mu,sigma)
for i in range(N-1):
    ynew = np.random.normal(mu,sigma)
    alpha = min(1,p(ynew)*q(y[i])/(p(y[i])*q(ynew)))
    if u[i] < alpha:
        y[i+1] = ynew
    else:
        y[i+1] = y[i]

# random walk chain
u2 = np.random.rand(N)
sigma = 10
y2 = np.zeros(N)
y2[0] = np.random.normal(0,sigma)
for i in range(N-1):
    y2new = y2[i] + np.random.normal(0,sigma)
    alpha = min(1,p(y2new)/p(y2[i]))
    if u2[i] < alpha:
        y2[i+1] = y2new
    else:
        y2[i+1] = y2[i]

plt.figure(1)
nbins = 30
plt.hist(y, bins = x)
plt.plot(x, px*N/np.sum(px), color='r', linewidth=2)

plt.show()
