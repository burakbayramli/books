## example10_4
from powell import *
from numarray import array
from math import sqrt

def F(x):
    lam = 1.0                  # Penalty multiplier
    c = x[0]*x[1] - 5.0            # Constraint equation
    return  distSq(x) + lam*c**2   # Penalized merit function

def distSq(x): return (x[0] - 5)**2 + (x[1] - 8)**2
    
xStart = array([1.0,5.0])
x,numIter = powell(F,xStart,0.01)
print "Intersection point =",x
print "Minimum distance =", sqrt(distSq(x))
print "xy =", x[0]*x[1]
print "Number of cycles =",numIter
raw_input ("Press return to exit")
