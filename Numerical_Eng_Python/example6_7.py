## example6_7
from math import cos,sqrt,pi
from romberg import *

def f(x): return 2.0*(x**2)*cos(x**2)

I,n = romberg(f,0,sqrt(pi))
print "Integral =",I
print "numEvals =",n
raw_input("\nPress return to exit")
