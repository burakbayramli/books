#!/usr/bin/python
## example7_2
from numarray import array, zeros,Float64
from printSoln import *
from taylor import *

def deriv(x,y):
    D = zeros((4,2),type=Float64)
    D[0] = [y[1]  , -0.1*y[1] - x]
    D[1] = [D[0,1],  0.01*y[1] + 0.1*x - 1.0]
    D[2] = [D[1,1], -0.001*y[1] - 0.01*x + 0.1]
    D[3] = [D[2,1],  0.0001*y[1] + 0.001*x - 0.01]
##    D[0,0] = y[1]
##    D[0,1] = -0.1*y[1] - x
##    D[1,0] = D[0,1]
##    D[1,1] = 0.01*y[1] + 0.1*x - 1.0
##    D[2,0] = D[1,1]
##    D[2,1] = -0.001*y[1] - 0.01*x + 0.1
##    D[3,0] = D[2,1]
##    D[3,1] = 0.0001*y[1] + 0.001*x - 0.01
    return D

x = 0.0               # Start of integration                 
xStop = 2.0           # End of integration                      
y = array([0.0, 1.0]) # Initial values of {y}  
h = 0.25              # Step size
freq = 1              # Printout frequency 
X,Y = taylor(deriv,x,y,xStop,h)
printSoln(X,Y,freq)
raw_input("\nPress return to exit")
