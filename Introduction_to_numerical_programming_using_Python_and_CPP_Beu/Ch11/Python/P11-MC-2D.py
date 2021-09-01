# Monte Carlo calculation of the unit circle area
from math import *
from random import *

# main

n = eval(input("n = "))                           # number of sampling points

seed()

ni = 0                                            # number of interior points
for i in range(1,n+1):
   x = random(); y = random()
   if (x*x + y*y <= 1e0): ni += 1                        # add interior point

fi = ni/n                                       # fraction of interior points
s = 4e0 * fi                                                       # integral
sigma = 4e0 * sqrt((fi - fi*fi)/n)                       # standard deviation
print("Unit circle area = ",s," +/- ",sigma)
