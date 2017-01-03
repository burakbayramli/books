
# Code from Chapter 12 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008


# The Genetic algorithm
# Comment and uncomment fitness functions as appropriate (as an import and the fitnessFunction variable)

from pylab import *
from numpy import *
import knapsack as fF

class ga:

	def __init__(self,stringLength,fitnessFunction,nEpochs,populationSize=100,mutationProb=-1,crossover='un',nElite=4,tournament=True):
		""" Constructor"""
		self.stringLength = stringLength
		
		# Population size should be even
		if mod(populationSize,2)==0:
			self.populationSize = populationSize
		else:
			self.populationSize = populationSize+1
		
		if mutationProb < 0:
			 self.mutationProb = 1/stringLength
		else:
			 self.mutationProb = mutationProb
			 	  
		self.nEpochs = nEpochs

		self.fitnessFunction = fitnessFunction

		self.crossover = crossover
		self.nElite = nElite
		self.tournment = tournament

		self.population = random.rand(self.populationSize,self.stringLength)
		self.population = where(self.population<0.5,0,1)
		
	def runGA(self):
		"""The basic loop"""
		ion()
		plotfig = figure
		bestfit = zeros(self.nEpochs)

		for i in range(self.nEpochs):
			# Compute fitness of the population
			fitness = eval(self.fitnessFunction)(self.population)

			# Pick parents -- can do in order since they are randomised
			newPopulation = self.fps(self.population,fitness)

			# Apply the genetic operators
			if self.crossover == 'sp':
				newPopulation = self.spCrossover(newPopulation)
			elif self.crossover == 'un':
				newPopulation = self.uniformCrossover(newPopulation)
			newPopulation = self.mutate(newPopulation)

			# Apply elitism and tournaments if using
			if self.nElite>0:
				newPopulation = self.elitism(self.population,newPopulation,fitness)
	
			if self.tournament:
				newPopulation = self.tournament(self.population,newPopulation,fitness,self.fitnessFunction)
	
			self.population = newPopulation
			bestfit[i] = fitness.max()

			if (mod(i,100)==0):
				print i, fitness.max()	
			#plot([i],[fitness.max()],'r+')
		plot(bestfit,'kx-')
		show()
	
	def fps(self,population,fitness):

		# Scale fitness by total fitness
		fitness = fitness/sum(fitness)
		fitness = 10*fitness/fitness.max()
		
		# Put repeated copies of each string in according to fitness
		# Deal with strings with very low fitness
		j=0
		while round(fitness[j])<1:
			j = j+1
		
		newPopulation = kron(ones((round(fitness[j]),1)),population[j,:])

		# Add multiple copies of strings into the newPopulation
		for i in range(j+1,self.populationSize):
			if round(fitness[i])>=1:
				newPopulation = concatenate((newPopulation,kron(ones((round(fitness[i]),1)),population[i,:])),axis=0)

		# Shuffle the order (note that there are still too many)
		indices = range(shape(newPopulation)[0])
		random.shuffle(indices)
		newPopulation = newPopulation[indices[:self.populationSize],:]
		return newPopulation	

	def spCrossover(self,population):
		# Single point crossover
		newPopulation = zeros(shape(population))
		crossoverPoint = random.randint(0,self.stringLength,self.populationSize)
		for i in range(0,self.populationSize,2):
			newPopulation[i,:crossoverPoint[i]] = population[i,:crossoverPoint[i]]
			newPopulation[i+1,:crossoverPoint[i]] = population[i+1,:crossoverPoint[i]]
			newPopulation[i,crossoverPoint[i]:] = population[i+1,crossoverPoint[i]:]
			newPopulation[i+1,crossoverPoint[i]:] = population[i,crossoverPoint[i]:]
		return newPopulation

	def uniformCrossover(self,population):
		# Uniform crossover
		newPopulation = zeros(shape(population))
		which = random.rand(self.populationSize,self.stringLength)
		which1 = which>=0.5
		for i in range(0,self.populationSize,2):
			newPopulation[i,:] = population[i,:]*which1[i,:] + population[i+1,:]*(1-which1[i,:])
			newPopulation[i+1,:] = population[i,:]*(1-which1[i,:]) + population[i+1,:]*which1[i,:]
		return newPopulation
		
	def mutate(self,population):
		# Mutation
		whereMutate = random.rand(shape(population)[0],shape(population)[1])
		population[where(whereMutate < self.mutationProb)] = 1 - population[where(whereMutate < self.mutationProb)]
		return population

	def elitism(self,oldPopulation,population,fitness):
		best = argsort(fitness)
		best = squeeze(oldPopulation[best[-self.nElite:],:])
		indices = range(shape(population)[0])
		random.shuffle(indices)
		population = population[indices,:]
		population[0:self.nElite,:] = best
		return population

	def tournament(self,oldPopulation,population,fitness,fitnessFunction):
		newFitness = eval(self.fitnessFunction)(population)
		for i in range(0,shape(population)[0],2):
			f = concatenate((fitness[i:i+2],newFitness[i:i+2]),axis=1)
			indices = argsort(f)
			if indices[-1]<2 and indices[-2]<2:
				population[i,:] = oldPopulation[i,:]
				population[i+1,:] = oldPopulation[i+1,:]
			elif indices[-1]<2:
				if indices[0]>=2:
					population[i+indices[0]-2,:] = oldPopulation[i+indices[-1]]
				else:
					population[i+indices[1]-2,:] = oldPopulation[i+indices[-1]]
			elif indices[-2]<2:
				if indices[0]>=2:
					population[i+indices[0]-2,:] = oldPopulation[i+indices[-2]]
				else:
					population[i+indices[1]-2,:] = oldPopulation[i+indices[-2]]
		return population
			
