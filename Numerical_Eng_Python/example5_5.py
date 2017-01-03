## example5_5
from numarray import array
from gaussPivot import *
from polyFit import *

xData = array([0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4])
yData = array([1.9934, 2.1465, 2.2129, 2.1790, \
                2.0683, 1.9448, 1.7655, 1.5891])
while 1:
    try:
        m = eval(raw_input("\nDegree of polynomial ==> "))
        coeff = polyFit(xData,yData,m)
        print "Coefficients are:\n",coeff
        print "Std. deviation =",stdDev(coeff,xData,yData)
    except SyntaxError: break
raw_input("Finished. Press return to exit")
