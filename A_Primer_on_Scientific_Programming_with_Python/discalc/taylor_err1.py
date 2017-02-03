#!/usr/bin/env python

from numpy import exp, abs

def q_h(h):
    return abs(exp(h) - (1+h))/h**2

print "  h     q_h"
for h in 0.1, 0.05, 0.01, 0.001:
    print "%5.3f %f" %(h, q_h(h))

