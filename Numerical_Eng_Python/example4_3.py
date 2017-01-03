#!/usr/bin/python
## example4_3
from math import tan
from rootsearch import *
from bisect import *

def f(x): return x - tan(x)

a,b,dx = (0.0, 20.0, 0.01)
print "The roots are:"
while 1:
    x1,x2 = rootsearch(f,a,b,dx)
    if x1 != None:
        a = x2
        root = bisect(f,x1,x2,1)
        if root != None: print root
    else:
        print "\nDone"
        break
raw_input("Press return to exit")
