#!/usr/bin/env python
from Numeric import *
import os, sys

# plot NumPy arrays in Matlab, if pymat is present:
try:
    import pymat
except:
    print "pymat module is not available..."; sys.exit(1)

x = arrayrange(0,4*math.pi,0.1)
m = pymat.open()
pymat.put(m, 'x', x);
pymat.eval(m, 'y = sin(x)')
pymat.eval(m, 'plot(x,y)')
y = pymat.get(m, 'y')
import time; time.sleep(4)  # wait before killing the plot...
pymat.close(m)

# compare sin(x) in Matlab and Python:
print "Matlab: sin(%g)=%g. Python: sin(%g)=%g" % \
      (x[1], sin(x[1]), x[1], y[1])
