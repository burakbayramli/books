
# Code from Chapter 12 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Population Based Incremental Learning algorithm
# Comment and uncomment fitness functions as appropriate (as an import and the fitnessFunction variable)

from pylab import *
from numpy import *
#import fourpeaks as fF
import knapsack as fF

def PBIL():
	ion()
	
	populationSize = 100
	stringLength = 20	
	eta = 0.005
	
	#fitnessFunction = 'fF.fourpeaks'
	fitnessFunction = 'fF.knapsack'
	p = 0.5*ones(stringLength)
	best = zeros(501,dtype=float)

	for count in range(501):
		# Generate samples
		population = random.rand(populationSize,stringLength)
		for i in range(stringLength):
			population[:,i] = where(population[:,i]<p[i],1,0)

		# Evaluate fitness
		fitness = eval(fitnessFunction)(population)

		# Pick best
		best[count] = max(fitness)
		bestplace = argmax(fitness)
		fitness[bestplace] = 0
		secondplace = argmax(fitness)

		# Update vector
		p  = p*(1-eta) + eta*((population[bestplace,:]+population[secondplace,:])/2)

		if (mod(count,100)==0):
			print count, best[count]

	plot(best,'kx-')
	xlabel('Epochs')
	ylabel('Fitness')
	show()
	#print p

PBIL()
