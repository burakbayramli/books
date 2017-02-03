#!/usr/bin/env python

def diff(f, x, h):
    return (f(x+h) - f(x))/float(h)

from math import *
import sys

try:
    x = eval(sys.argv[1])
    h = eval(sys.argv[2])
except:
    print "usage: %s x h" % sys.argv[0]
    sys.exit(1)

approx_deriv = diff(sin, x, h)
exact = cos(x)
print 'The approximated value is: ', approx_deriv
print 'The correct value is:      ', exact
print 'The error is:              ', exact - approx_deriv
