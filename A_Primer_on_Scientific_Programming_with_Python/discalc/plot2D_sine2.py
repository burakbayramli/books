#!/usr/bin/env python

from scitools.std import *

try:
    n = int(sys.argv[1])
except:
    print "usage: %s n" %sys.argv[0]
    sys.exit(1)

x = y = linspace(0, 1, n+1)
xv, yv = ndgrid(x, y)
s = sin(pi*xv)*sin(pi*yv)
c = linspace(0, 1, 100)
contourf(xv, yv, s, c)
colorbar()
