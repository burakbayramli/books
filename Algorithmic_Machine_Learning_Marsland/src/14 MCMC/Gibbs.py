
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A simple Gibbs sampler
from pylab import *
from numpy import *

def pxgiveny(y,mx,my,s1,s2):
    return random.normal(mx + (y-my)/s2,s1)
    #return random.binomial(16,y,1)

def pygivenx(x,mx,my,s1,s2):
    return random.normal(my + (x-mx)/s1,s2)
    #return random.beta(x+2,16-x+4,1)

def gibbs(N=500):
    k=10
    x0 = zeros(N,dtype=float)
    m1 = 10
    m2 = 20
    s1 = 2
    s2 = 3
    for i in range(N):
        y = random.rand(1)
        for j in range(k):
            x = pxgiveny(y,m1,m2,s1,s2)
            y = pygivenx(x,m1,m2,s1,s2)
        x0[i] = x
    
    return x0

#def f(x):
#    n = 16
#    alph = 2
#    bet = 4
#    return 20.0*(factorial(n)/(factorial(x)*factorial(n-x)))*factorial(x+1)*factorial(19-x)/factorial(21)
#
#def factorial(n):
#    x = 1
#    for i in range(n):
#        x *= (i+1)
#    return x

def f(x):
    return exp(-(x-10)**2/10)

N=500
s=gibbs(N)
x1 = arange(0,17,1)
hist(s,bins=x1,fc='k')
x1 = arange(0,17,0.1)
px1 = zeros(len(x1))
for i in range(len(x1)):
    px1[i] = f(x1[i])
plot(x1, px1*N*10/sum(px1), color='k',linewidth=3)

show()
