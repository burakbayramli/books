#!/usr/bin/env python

def diff2D(f, x, y, h):
    dfx = (f(x+h, y) - f(x, y))/h
    dfy = (f(x, y+h) - f(x, y))/h
    return dfx, dfy

from scitools.StringFunction import StringFunction
import sys
try:
    f_formula = sys.argv[1]
    x = eval(sys.argv[2])
    y = eval(sys.argv[3])
except:
    print "usage: %s 'f(x)' x y h" %sys.argv[0]
    sys.exit(1)
# read h from the command line if present:
try:
    h = eval(sys.argv[4])
except:
    h = 1.0E-5  # default value

f = StringFunction(f_formula, independent_variables=('x','y'))
dfx, dfy = diff2D(f, x, y, h)
print 'df/dx:', dfx
print 'df/dy:', dfy
