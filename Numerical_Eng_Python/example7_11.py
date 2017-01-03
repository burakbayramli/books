## example7_11
from bulStoer import *
from numarray import array,zeros,Float64
from printSoln import *

def F(x,y):
    F = zeros((2),type=Float64) 
    F[0] = y[1]
    F[1] =(-y[1] - y[0]/0.45 + 9.0)/2.0
    return F

H = 0.5
xStop = 10.0
x = 0.0
y = array([0.0, 0.0])
X,Y = bulStoer(F,x,y,xStop,H)
printSoln(X,Y,1)
raw_input("\nPress return to exit")


