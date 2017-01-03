
# Code from Chapter 15 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A basic Hidden Markov Model
from numpy import *

def HMMfwd(a,b,obs):

	nStates = shape(b)[0]
	T = shape(obs)[0]

	alpha = zeros((nStates,T))

	alpha[:,0] = aFirst*b[:,obs[0]]

	for t in range(1,T):
		for s in range(nStates):
			alpha[s,t] = b[s,obs[t]] * sum(alpha[:,t-1] * a[:,s])

	#alpha[q,T] = sum(alpha[:,T-1]*aLast[:,q])
	#print max(alpha[:,T-1])
	return alpha

def HMMbwd(a,b,obs):

	nStates = shape(b)[0]
	T = shape(obs)[0]

	beta = zeros((nStates,T))

	beta[:,T-1] = aLast

	for t in range(T-2,0,-1):
		for s in range(nStates):
			beta[s,t] = b[s,obs[t+1]] * sum(beta[:,t+1] * a[:,s])

	beta[:,0] = b[:,obs[0]] * sum(beta[:,1] * aFirst)
	return beta

def BaumWelch(obs,nStates):

	T = shape(obs)[0]
	a = random.rand(nStates,nStates)
	b = random.rand(nStates,T)
	olda = zeros((nStates,nStates)) 
	oldb = zeros((nStates,T)) 
	maxCount = 50
	tolerance = 1e-5

	count = 0
	while (abs(a-olda)).max() > tolerance and (abs(b-oldb)).max() > tolerance and count < maxCount:
		# E-step

		alpha = HMMfwd(a,b,obs)
		beta = HMMbwd(a,b,obs)
		gamma = zeros((nStates,nStates,T))

		for t in range(T-1):
			for s in range(nStates):
				gamma[:,s,t] = alpha[:,t] * a[:,s] * b[s,obs[t+1]] * beta[s,t+1] / max(alpha[:,T-1])
	
		# M-step
		olda = a.copy()
		oldb = b.copy()

		for i in range(nStates):
			for j in range(nStates):
				a[i,j] = sum(gamma[i,j,:])/sum(sum(gamma[i,:,:]))

		for o in range(max(obs)):
			for j in range(nStates):
				places = (obs==o).nonzero()
				tally = sum(gamma[j,:,:],axis=0)
				b[j,o] = sum(tally[places])/sum(sum(gamma[j,:,:]))
				#print b[j,o], sum(gamma[j,places])/sum(gamma[j,:])
	
		count += 1
	print count
	return a,b

def ViterbiSimple(a,b,obs):

	nStates = shape(b)[0]
	T = shape(obs)[0]

	path = zeros(T)
	viterbi = zeros((nStates,T))

	viterbi[:,0] = aFirst * b[:,obs[0]]
	path[0] = argmax(viterbi[:,0])

	for t in range(1,T):
		for s in range(nStates):
			viterbi[s,t] = max(viterbi[:,t-1] * a[:,s] * b[s,obs[t]])
		path[t] = argmax(viterbi[:,t])

	print "Path: ",  path
	#print viterbi[path[T-1]]
	return path,viterbi

def Viterbi(a,b,obs):

	nStates = shape(b)[0]
	T = shape(obs)[0]

	path = zeros(T)
	backwards = zeros((nStates,T))
	viterbi = zeros((nStates,T))

	viterbi[:,0] = aFirst * b[:,obs[0]]
	backwards[:,1] = 0

	for t in range(1,T):
		for s in range(nStates):
			tally = viterbi[:,t-1] * a[:,s] * b[s,obs[t]]
			backwards[s,t] = argmax(tally)
			viterbi[s,t] = tally[backwards[s,t]]
		path[t] = argmax(viterbi[:,t])

	print path
	return path,viterbi,backwards


aFirst = array([0.25,0.25,0.25,0.25])
aLast = array([0.25,0.25,0.25,0.25])
#a = array([[.7,.3],[.4,.6]] )
a = array([[.4,.3,.1,.2],[.6,.05,.1,.25],[.7,.05,.05,.2],[.3,.4,.25,.05]])
#a = a.transpose()
#b = array([[.2,.4,.4],[.5,.4,.1]] )
b = array([[.2,.1,.2,.5],[.4,.2,.1,.3],[.3,.4,.2,.1],[.3,.05,.3,.35]])
obs = array([0,0,3,1,1,2,1,3])
#obs = array([2,0,2])
HMMfwd(a,b,obs)
Viterbi(a,b,obs)
ViterbiSimple(a,b,obs)
BaumWelch(obs,4)
ViterbiSimple(a,b,obs)
