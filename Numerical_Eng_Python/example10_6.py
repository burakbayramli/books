## example10_6
from fletcherReeves import *
from numarray import array,zeros,Float64
from math import cos,tan,pi

def F(x):
    return  8.0/x[0] - x[0]*(tan(x[1]) - 2.0/cos(x[1]))

def gradF(x):
    g = zeros((2),type=Float64)
    g[0] = -8.0/(x[0]**2) - tan(x[1]) + 2.0/cos(x[1])
    g[1] = x[0]*(-1.0/cos(x[1]) + 2.0*tan(x[1]))/cos(x[1])
    return g

x = array([2.0, 0.0])
x,nIter = optimize(F,gradF,x)
b = 8.0/x[0] - x[0]*tan(x[1])
print "h =",x[0],"m"
print "b =",b,"m"
print "theta =",x[1]*180.0/pi,"deg"
print "perimeter =", F(x),"m"
print "Number of iterations =",nIter
raw_input("Press return to exit")
