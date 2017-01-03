
# Code from Chapter 8 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Plots of three 2D Gaussians

from pylab import *
from numpy import *

x = arange(-5,5,0.01)
s = 1
mu = 0
y = 1/(sqrt(2*pi)*s) * exp(-0.5*(x-mu)**2/s**2)
plot(x,y,'k')

close('all')
mu = array([2,-3])
s = array([1,1])
#s = array([0.5,2])
x = random.normal(mu,scale=s,size = (500,2))
plot(x[:,0],x[:,1],'ko')
#axis(array([0,3,-8,4]))
axis('equal')

theta = arange(0,2.1*pi,pi/20)

plot(mu[0]+2*cos(theta),mu[1]+2*sin(theta),'k-')
plot(mu[0]+3*cos(theta),mu[1]+3*sin(theta),'k-')


figure()

mu = array([2,-3])
s = array([0.5,2])
x = random.normal(mu,scale=s,size = (500,2))
phi = 2*pi/3
plot(x[:,0]*cos(phi)+x[:,1]*sin(phi),x[:,0]*(-sin(phi)) + x[:,1]*cos(phi),'ko')
axis('equal')

theta = arange(0,2.1*pi,pi/20)
plot((mu[0]+3*s[0]*cos(theta))*cos(phi)+(mu[1]+3*s[1]*sin(theta))*sin(phi), (mu[0]+3*s[0]*cos(theta))*sin(-phi)+(mu[1]+3*s[1]*sin(theta))*cos(phi), 'k-')

figure()
mu = array([2,-3])
s = array([0.5,2])
x = random.normal(mu,scale=s,size = (500,2))
plot(x[:,0],x[:,1],'ko')
axis('equal')

theta = arange(0,2.1*pi,pi/20)
plot(mu[0]+3*s[0]*cos(theta),mu[1]+3*s[1]*sin(theta), 'k-')

show()