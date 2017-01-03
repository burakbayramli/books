## example7_6
from numarray import zeros,Float64,array
from run_kut4 import *
from printSoln import *

def F(x,y):                     
    F = zeros((4),type=Float64) 
    F[0] = y[1]
    F[1] = y[0]*(y[3]**2) - 3.9860e14/(y[0]**2)
    F[2] = y[3]
    F[3] = -2.0*y[1]*y[3]/y[0]
    return F
  
x = 0.0
xStop = 1200.0
y = array([7.15014e6, 0.0, 0.0, 0.937045e-3])
h = 50.0
freq = 2

X,Y = integrate(F,x,y,xStop,h)
printSoln(X,Y,freq)
raw_input("\nPress return to exit")
