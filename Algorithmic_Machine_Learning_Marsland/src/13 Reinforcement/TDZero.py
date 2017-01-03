
# Code from Chapter 13 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The basic TD(0) algorithm with the Europe example

from numpy import *

def TDZero():

	R = array([[-5,0,-inf,-inf,-inf,-inf],[0,-5,0,0,-inf,-inf],[-inf,0,-5,0,-inf,100],[-inf,0,0,-5,0,-inf],[-inf,-inf,-inf,0,-5,100],[-inf,-inf,0,-inf,-inf,0]])
	t = array([[1,1,0,0,0,0],[1,1,1,1,0,0],[0,1,1,1,0,1],[0,1,1,1,1,0],[0,0,0,1,1,1],[0,0,1,0,1,1]])

	nStates = shape(R)[0]
	nActions = shape(R)[1]
	Q = random.rand(nStates,nActions)*0.1-0.05
	mu = 0.7
	gamma = 0.4
	epsilon = 0.1
	nits = 0

	while nits < 1000:
		# Pick initial state
		s = random.randint(nStates)
		# Stop when the accepting state is reached
		while s!=5:
			# epsilon-greedy
			if (random.rand()<epsilon):
				indices = where(t[s,:]!=0)
				pick = random.randint(shape(indices)[1])
				a = indices[0][pick]
				#print s,a
			else:
				a = argmax(Q[s,:])

			r = R[s,a]
			# For this example, new state is the chosen action
			sprime = a
			#print "here"
			Q[s,a] += mu * (r + gamma*max(Q[sprime,:]) - Q[s,a])
			s = sprime

		nits = nits+1

	print Q

TDZero()
