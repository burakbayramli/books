#!/usr/bin/env python

from numpy import *
import sys

try:
    n = int(sys.argv[1])
except:
    print "usage: %s n" %sys.argv[0]
    sys.exit(1)

h = 1./n
x = y = linspace(0, 1, n+1)
xv, yv = ndgrid(x, y)
s = sin(pi*xv)*sin(pi*yv)
I = 0
for j in xrange(len(y)-1):
    for i in xrange(len(x)-1):
        I += (s[i, j] + s[i+1, j] + s[i, j+1] + s[i+1, j+1])

I *= 1.0/(4*n**2)
print 'Approximate integral:', I

