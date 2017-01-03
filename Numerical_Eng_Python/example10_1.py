## example10_1
from goldSearch import *

def f(x):
    lam = 1.0        # Constraint multiplier
    c = min(0.0, x)  # Constraint function
    return 1.6*x**3 + 3.0*x**2 - 2.0*x + lam*c**2
    
xStart = 1.0
h = 0.01
x1,x2 = bracket(f,xStart,h)
x,fMin = search(f,x1,x2)
print "x =",x
print "f(x) =",fMin
raw_input ("\nPress return to exit")
