#!/usr/bin/python
## example3_7
from numarray import array,Float64
from cubicSpline import *

xData = array([1,2,3,4,5],type=Float64)
yData = array([0,1,0,1,0],type=Float64)
k = curvatures(xData,yData)
while 1:
    try: x = eval(raw_input("\nx ==> "))
    except SyntaxError: break
    print "y =",evalSpline(xData,yData,k,x)
raw_input("Done. Press return to exit")
