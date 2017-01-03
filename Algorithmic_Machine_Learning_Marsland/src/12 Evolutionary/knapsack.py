
# Code from Chapter 12 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A fitness function for the Knapsack problem
from numpy import *

def knapsack(pop):
	maxSize = 500	
	#sizes = array([193.71,60.15,89.08,88.98,15.39,238.14,68.78,107.47,119.66,183.70])

 	sizes = array([109.60,125.48,52.16,195.55,58.67,61.87,92.95,93.14,155.05,110.89,13.34,132.49,194.03,121.29,179.33,139.02,198.78,192.57,81.66,128.90])

	fitness = sum(sizes*pop,axis=1)
	fitness = where(fitness>maxSize,500-2*(fitness-maxSize),fitness)
		
	return fitness
