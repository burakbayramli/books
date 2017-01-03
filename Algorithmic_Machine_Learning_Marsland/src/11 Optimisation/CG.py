
# Code from Chapter 11 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The conjugate gradients algorithm
from numpy import *

def Jacobian(x):
	#return array([.4*x[0],2*x[1]])
	return array([x[0], 0.4*x[1], 1.2*x[2]])

def Hessian(x):
	#return array([[.2,0],[0,1]])
	return array([[1,0,0],[0,0.4,0],[0,0,1.2]])

def CG(x0):

	i=0
	k=0

	r = -Jacobian(x0)
	p=r

	betaTop = dot(r.transpose(),r)
	beta0 = betaTop

	iMax = 3
	epsilon = 10**(-2)
	jMax = 5

	# Restart every nDim iterations
	nRestart = shape(x0)[0]
	x = x0

	while i < iMax and betaTop > epsilon**2*beta0:
		j=0
		dp = dot(p.transpose(),p)
		alpha = (epsilon+1)**2
		# Newton-Raphson iteration
		while j < jMax and alpha**2 * dp > epsilon**2:
			# Line search
			alpha = -dot(Jacobian(x).transpose(),p) / (dot(p.transpose(),dot(Hessian(x),p)))
			print "N-R",x, alpha, p
			x = x + alpha * p
			j += 1
		print x
		# Now construct beta
		r = -Jacobian(x)
		print "r: ", r
		betaBottom = betaTop
		betaTop = dot(r.transpose(),r)
		beta = betaTop/betaBottom
		print "Beta: ",beta
		# Update the estimate
		p = r + beta*p
		print "p: ",p
		print "----"
		k += 1
		
		if k==nRestart or dot(r.transpose(),p) <= 0:
			p = r
			k = 0
			print "Restarting"
		i +=1

	print x

x0 = array([-2,2,-2])
CG(x0)
