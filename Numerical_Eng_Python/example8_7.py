## example8_7
from numarray import zeros,Float64,array,arange
from newtonRaphson2 import *

def residual(y):  # Residuals of finite diff. Eqs. (8.11)
    r = zeros((m + 1),type=Float64)
    r[0] = y[0]
    r[m] = y[m] - 1.0
    for i in range(1,m):
        r[i] = y[i-1] - 2.0*y[i] + y[i+1]                      \
             - h*h*F(x[i],y[i],(y[i+1] - y[i-1])/(2.0*h))
    return r                 
        
def F(x,y,yPrime):  # Differential eqn. y" = F(x,y,y')
    F = -3.0*y*yPrime
    return F

def startSoln(x): # Starting solution y(x)
    y = zeros((m + 1),type=Float64)
    for i in range(m + 1): y[i] = 0.5*x[i]
    return y
 
xStart = 0.0            # x at left end
xStop = 2.0             # x at right end
m = 10                  # Number of mesh intevals
h = (xStop - xStart)/m
x = arange(xStart,xStop + h,h)
y = newtonRaphson2(residual,startSoln(x),1.0e-5)
print "\n        x              y"
for i in range(m + 1):
    print "%14.5e %14.5e" %(x[i],y[i]) 
raw_input("\nPress return to exit")


