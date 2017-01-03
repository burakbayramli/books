#!/usr/bin/python
## example3_4
from numarray import array,arange
from math import pi,cos
from newtonPoly import *

xData = array([0.15,2.3,3.15,4.85,6.25,7.95])
yData = array([4.79867,4.49013,4.2243,3.47313,2.66674,1.51909])
a = coeffts(xData,yData)
print " x    yInterp   yExact"
print "-----------------------"
for x in arange(0.0,8.1,0.5):
    y = evalPoly(a,xData,x)
    yExact = 4.8*cos(pi*x/20.0)
    print "%3.1f %9.5f %9.5f"% (x,y,yExact)
raw_input("\nPress return to exit")


