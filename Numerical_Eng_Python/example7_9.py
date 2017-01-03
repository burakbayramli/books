## example7_9
from numarray import array,zeros,Float64
from run_kut5 import *
from printSoln import *

def F(x,y):
    F = zeros((2),type=Float64) 
    F[0] = y[1]
    F[1] = -4.75*y[0] - 10.0*y[1]
    return F

x = 0.0
xStop = 10.0
y = array([-9.0, 0.0])
h = 0.1
freq = 4
X,Y = integrate(F,x,y,xStop,h)
printSoln(X,Y,freq)
raw_input("\nPress return to exit")
