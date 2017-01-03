## example10_3
from powell import *
from numarray import array

def F(x): return 100.0*(x[1] - x[0]**2)**2 + (1 - x[0])**2 

xStart = array([-1.0, 1.0])
xMin,nIter = powell(F,xStart)
print "x =",xMin
print "F(x) =",F(xMin)
print "Number of cycles =",nIter
raw_input ("Press return to exit")
