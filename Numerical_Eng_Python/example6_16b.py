## problem6_3_13
from numarray import array
from math import sqrt
from triangleQuad import *
    
def f(x,y):
    return (x**2 + y**2)/2.0        \
           -(x**3 - 3.0*x*y**2)/6.0 \
           -2.0/3.0

xCorner = array([-1.0, -1.0, 2.0])
yCorner = array([sqrt(3.0), -sqrt(3.0), 0.0])
print "Integral =",triangleQuad(f,xCorner,yCorner)
raw_input("Press return to  exit")
