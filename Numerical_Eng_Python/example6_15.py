## example 6_15
from gaussQuad2 import *
from numarray import array

def f(x,y): return ((x - 2.0)**2)*((y - 2.0)**2)

x = array([0.0, 4.0, 4.0, 1.0])
y = array([0.0, 1.0, 4.0, 3.0])
m = eval(raw_input("Integration order ==> "))
print "Integral =", gaussQuad2(f,x,y,m)
raw_input("\nPress return to exit")
