## example7_8
from numarray import array,zeros,Float64
from run_kut5 import *
from printSoln import *
from math import exp

def F(x,y):
    F = zeros((2),type=Float64) 
    F[0] = y[1]
    F[1] = -9.80665 + 65.351e-3 * y[1]**2 * exp(-10.53e-5*y[0])
    return F

x = 0.0
xStop = 10.0
y = array([9000, 0.0])
h = 0.5
freq = 1
X,Y = integrate(F,x,y,xStop,h,1.0e-2)
printSoln(X,Y,freq)
raw_input("\nPress return to exit")
