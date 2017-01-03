## example6_16a
from gaussQuad2 import *
from numarray import array
from math import sqrt

def f(x,y):
    return (x**2 + y**2)/2.0          \
           - (x**3 - 3.0*x*y**2)/6.0  \
           - 2.0/3.0

x = array([-1.0,-1.0,2.0,2.0])
y = array([sqrt(3.0),-sqrt(3.0),0.0,0.0])
m = eval(raw_input("Integration order ==> "))
print "Integral =", gaussQuad2(f,x,y,m)
raw_input("\nPress return to exit")
