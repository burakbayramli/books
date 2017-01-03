
# Code from Chapter 11 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A demonstration of four methods of solving the Travelling Salesman Problem
from numpy import *

def makeTSP(nCities):
	positions = 2*random.rand(nCities,2)-1;
	distances = zeros((nCities,nCities))

	for i in range(nCities):
		for j in range(i+1,nCities):
			distances[i,j] = sqrt((positions[i,0] - positions[j,0])**2 + (positions[i,1] - positions[j,1])**2);
			distances[j,i] = distances[i,j];

	return distances

def exhaustive(distances):
	nCities = shape(distances)[0]

	cityOrder = arange(nCities)

	distanceTravelled = 0
	for i in range(nCities-1):
		distanceTravelled += distances[cityOrder[i],cityOrder[i+1]]
	distanceTravelled += distances[cityOrder[nCities-1],0]

	for newOrder in permutation(range(nCities)):
		possibleDistanceTravelled = 0
		for i in range(nCities-1):
			possibleDistanceTravelled += distances[newOrder[i],newOrder[i+1]]
		possibleDistanceTravelled += distances[newOrder[nCities-1],0]
			 
		if possibleDistanceTravelled < distanceTravelled:
			distanceTravelled = possibleDistanceTravelled
			cityOrder = newOrder

	return cityOrder, distanceTravelled
	
def permutation(order):
	order = tuple(order)
	if len(order)==1:
		yield order
	else:
		for i in range(len(order)):
			rest = order[:i] + order[i+1:]
			move = (order[i],)
			for smaller in permutation(rest):
				yield move + smaller
		
def greedy(distances):
	nCities = shape(distances)[0]
	distanceTravelled = 0
	
	# Need a version of the matrix we can trash
	dist = distances.copy()

	cityOrder = zeros(nCities)
	cityOrder[0] = random.randint(nCities)
	dist[:,cityOrder[0]] = Inf

	for i in range(nCities-1):
		cityOrder[i+1] = argmin(dist[cityOrder[i],:])
		distanceTravelled  += dist[cityOrder[i],cityOrder[i+1]]
		# Now exclude the chance of travelling to that city again
		dist[:,cityOrder[i+1]] = Inf
	
	# Now return to the original city
	distanceTravelled += distances[cityOrder[nCities-1],0]

	return cityOrder, distanceTravelled

def hillClimbing(distances):

	nCities = shape(distances)[0]

	cityOrder = arange(nCities)
	random.shuffle(cityOrder)

	distanceTravelled = 0
	for i in range(nCities-1):
		distanceTravelled += distances[cityOrder[i],cityOrder[i+1]]
	distanceTravelled += distances[cityOrder[nCities-1],0]

	for i in range(1000):
		# Choose cities to swap
		city1 = random.randint(nCities)
		city2 = random.randint(nCities)

		if city1 != city2:
			# Reorder the set of cities
			possibleCityOrder = cityOrder.copy()
			possibleCityOrder = where(possibleCityOrder==city1,-1,possibleCityOrder)
			possibleCityOrder = where(possibleCityOrder==city2,city1,possibleCityOrder)
			possibleCityOrder = where(possibleCityOrder==-1,city2,possibleCityOrder)

			# Work out the new distances
			# This can be done more efficiently
			newDistanceTravelled = 0
			for j in range(nCities-1):
				newDistanceTravelled += distances[possibleCityOrder[j],possibleCityOrder[j+1]]
			distanceTravelled += distances[cityOrder[nCities-1],0]
	
			if newDistanceTravelled < distanceTravelled:
				distanceTravelled = newDistanceTravelled
				cityOrder = possibleCityOrder

	return cityOrder, distanceTravelled
	

def simulatedAnnealing(distances):

	nCities = shape(distances)[0]

	cityOrder = arange(nCities)
	random.shuffle(cityOrder)

	distanceTravelled = 0
	for i in range(nCities-1):
		distanceTravelled += distances[cityOrder[i],cityOrder[i+1]]
	distanceTravelled += distances[cityOrder[nCities-1],0]

	T = 500
	c = 0.8
	nTests = 10

	while T>1:
		for i in range(nTests):
			# Choose cities to swap
			city1 = random.randint(nCities)
			city2 = random.randint(nCities)

			if city1 != city2:
				# Reorder the set of cities
				possibleCityOrder = cityOrder.copy()
				possibleCityOrder = where(possibleCityOrder==city1,-1,possibleCityOrder)
				possibleCityOrder = where(possibleCityOrder==city2,city1,possibleCityOrder)
				possibleCityOrder = where(possibleCityOrder==-1,city2,possibleCityOrder)

				# Work out the new distances
				# This can be done more efficiently
				newDistanceTravelled = 0
				for j in range(nCities-1):
					newDistanceTravelled += distances[possibleCityOrder[j],possibleCityOrder[j+1]]
				distanceTravelled += distances[cityOrder[nCities-1],0]

				if newDistanceTravelled < distanceTravelled or (distanceTravelled - newDistanceTravelled) < T*log(random.rand()):
					distanceTravelled = newDistanceTravelled
					cityOrder = possibleCityOrder

			# Annealing schedule
			T = c*T

	return cityOrder, distanceTravelled

def runAll():
	import time

	nCities = 5
	distances = makeTSP(nCities)

	print "Exhaustive search"
	start = time.time()
	print exhaustive(distances)
	finish = time.time()
	print finish-start

	print "Greedy search"
	start = time.time()
	print greedy(distances)
	finish = time.time()
	print finish-start

	print "Hill Climbing"
	start = time.time()
	print hillClimbing(distances)
	finish = time.time()
	print finish-start

	print "Simulated Annealing"
	start = time.time()
	print simulatedAnnealing(distances)
	finish = time.time()
	print finish-start

runAll()