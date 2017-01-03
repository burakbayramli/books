## module plotPoly
''' plotPoly(xData,yData,coeff)
    Plots data points and the fitting
    polynomial defined by its coefficient
    array {coeff} = {a0, a1. ...}
'''    
from numarray import zeros,arange
from xyPlot import *

def plotPoly(xData,yData,coeff):
    m = len(coeff)
    x1 = min(xData)
    x2 = max(xData)
    dx = (x2 - x1)/20.0   
    x = arange(x1,x2 + dx/10.0,dx)
    y = zeros((len(x)))*1.0
    for i in range(m):
        y = y + coeff[i]*x**i
    xyPlot(xData,yData,'no',x,y,'ln') 
