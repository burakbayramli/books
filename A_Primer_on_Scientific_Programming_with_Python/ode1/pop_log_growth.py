#!/usr/bin/env python

import numpy as np
import sys

try:
    t1 = int(sys.argv[1])
except:
    print "usage:", sys.argv[0], "n (number of years)"
    sys.exit(1)

t0 = 1750
u0 = 2
R = 950
t = np.linspace(t0, t0 + t1 + .5, t1)
u = np.zeros(t1 + 1)
a = 0.0218
u[0] = u0
for i in range(len(u) - 1):
    u[i+1] = u[i] - a*u[i]*(1 - u[i]/R)
print "Expected population in year %d is" %(t0 + t1), u[-1]
