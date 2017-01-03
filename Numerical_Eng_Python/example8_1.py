## example8_1
from numarray import zeros,Float64,array
from run_kut4 import *
from brent import *
from printSoln import *

def initCond(u):  # Init. values of [y,y']; use 'u' if unknown
    return array([0.0, u])  

def r(u):         # Boundary condition residual--see Eq. (8.3)
    X,Y = integrate(F,xStart,initCond(u),xStop,h)
    y = Y[len(Y) - 1]
    r = y[0] - 1.0 
    return r

def F(x,y):       # First-order differential equations
    F = zeros((2),type=Float64)
    F[0] = y[1]
    F[1] = -3.0*y[0]*y[1]
    return F

xStart = 0.0        # Start of integration
xStop = 2.0         # End of integration
u1 = 1.0            # 1st trial value of unknown init. cond.
u2 = 2.0            # 2nd trial value of unknown init. cond.
h = 0.1             # Step size
freq = 2            # Printout frequency
u = brent(r,u1,u2)  # Compute the correct initial condition
X,Y = integrate(F,xStart,initCond(u),xStop,h)
printSoln(X,Y,freq)
raw_input("\nPress return to exit")
