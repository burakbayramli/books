
# Code from Chapter 8 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A k-Nearest Neighbour Classifier
from numpy import *

def knn(k,data,dataClass,inputs):

	nInputs = shape(inputs)[0]
	closest = zeros(nInputs)

	for n in range(nInputs):
		# Compute distances
		distances = sum((data-inputs[n,:])**2,axis=1)

		# Identify the nearest neighbours
		indices = argsort(distances,axis=0)

		classes = unique(dataClass[indices[:k]])
		if len(classes)==1:
			closest[n] = unique(classes)
		else:
			counts = zeros(max(classes)+1)
			for i in range(k):
				counts[dataClass[indices[i]]] += 1
			closest[n] = max(counts)
			 
	return closest
