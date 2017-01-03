
# Code from Chapter 2 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from numpy import *

def linreg(inputs,targets):

	inputs = concatenate((inputs,-ones((shape(inputs)[0],1))),axis=1)
	beta = dot(dot(linalg.inv(dot(transpose(inputs),inputs)),transpose(inputs)),targets)

	outputs = dot(inputs,beta)
	#print shape(beta)
	#print outputs
	return beta
