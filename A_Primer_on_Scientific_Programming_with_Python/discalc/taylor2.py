#!/usr/bin/env python

from math import exp
import sys
try:
    h = float(sys.argv[1])
except:
    print '%s h' % sys.argv[0]
    sys.exit(1)
    
Taylor_series = []
Taylor_series.append(1)
Taylor_series.append(Taylor_series[-1] + h)
Taylor_series.append(Taylor_series[-1] + (1/2.0)*h**2)
Taylor_series.append(Taylor_series[-1] + (1/6.0)*h**3)
Taylor_series.append(Taylor_series[-1] + (1/24.0)*h**4)

print 'h =', h
for order in range(len(Taylor_series)):
    print 'order=%d, error=%g' % \
          (order, exp(h) - Taylor_series[order])


