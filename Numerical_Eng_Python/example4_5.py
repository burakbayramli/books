#!/usr/bin/python
## example4_5
from math import cos
from brent import *

def f(x): return x*abs(cos(x)) - 1.0

print "root =",brent(f,0.0,4.0)
raw_input("Press return to exit")
