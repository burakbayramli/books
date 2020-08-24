from numpy import *
from time import *

#import psyco
#psyco.full()

a = time()
for x in range(5, 1000000, 2):
	prime = 0
	for y in range(3,int(sqrt(x))+1,2):
		if x%y == 0:
			prime = 1
			break
	if prime == 0:
		print x
	
print time() - a