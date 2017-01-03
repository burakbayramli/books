
# Code from Chapter 2 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

from numpy import *

class pcn:
	""" A basic Perceptron (the same pcn.py except with the weights printed
	and it does not reorder the inputs)"""
	
	def __init__(self,inputs,targets):
		""" Constructor """
		# Set up network size
		if ndim(inputs)>1:
			self.nIn = shape(inputs)[1]
		else: 
			self.nIn = 1
	
		if ndim(targets)>1:
			self.nOut = shape(targets)[1]
		else:
			self.nOut = 1

		self.nData = shape(inputs)[0]
	
		# Initialise network
		self.weights = random.rand(self.nIn+1,self.nOut)*0.1-0.05

	def pcntrain(self,inputs,targets,eta,nIterations):
		""" Train the thing """	
		# Add the inputs that match the bias node
		inputs = concatenate((inputs,-ones((self.nData,1))),axis=1)
	
		# Training
		change = range(self.nData)

		for n in range(nIterations):
			
			self.outputs = self.pcnfwd(inputs);
			self.weights += eta*dot(transpose(inputs),targets-self.outputs)
			print "Iteration: ", n
			print self.weights
			
			activations = self.pcnfwd(inputs)
			print "Final outputs are:"
			print activations
		#return self.weights

	def pcnfwd(self,inputs):
		""" Run the network forward """

		outputs =  dot(inputs,self.weights)

		# Threshold the outputs
		return where(outputs>0,1,0)


	def confmat(self,inputs,targets):
		"""Confusion matrix"""

		# Add the inputs that match the bias node
		inputs = concatenate((inputs,-ones((self.nData,1))),axis=1)
		outputs = dot(inputs,self.weights)
	
		nClasses = shape(targets)[1]

		if nClasses==1:
			nClasses = 2
			outputs = where(outputs>0,1,0)
		else:
			# 1-of-N encoding
			outputs = argmax(outputs,1)
			targets = argmax(targets,1)

		cm = zeros((nClasses,nClasses))
		for i in range(nClasses):
			for j in range(nClasses):
				cm[i,j] = sum(where(outputs==i,1,0)*where(targets==j,1,0))

		print cm
		print trace(cm)/sum(cm)


