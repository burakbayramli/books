#!/usr/bin/env python

def trapezoidal(f, a, b, n):
    x = linspace(a, b, n+1)
    values = f(x)
    values[0] /= 2.0
    values[-1] /= 2.0
    h = (b-a)/float(n)
    values *= h
    I = sum(values)
    return I

from scitools.std import *
try:
    f_formula = sys.argv[1]
    a = eval(sys.argv[2])
    b = eval(sys.argv[3])
    n = int(sys.argv[4])
except:
    print "usage: %s 'f(x)' a b n" %sys.argv[0]
    sys.exit(1)

f = StringFunction(f_formula)
f.vectorize(globals())
I = trapezoidal(f, a, b, n)
print 'Approximation of the integral: ', I
