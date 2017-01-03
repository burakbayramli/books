## example6_4
from math import sqrt,cos,pi
from trapezoid import *

def f(x): return sqrt(x)*cos(x)

Iold = 0.0
for k in range(1,21):
    Inew = trapezoid(f,0.0,pi,Iold,k)
    if (k > 1) and (abs(Inew - Iold)) < 1.0e-6: break
    Iold = Inew
print "Integral =",Inew
print "nPanels =",2**(k-1)
raw_input("\nPress return to exit")




