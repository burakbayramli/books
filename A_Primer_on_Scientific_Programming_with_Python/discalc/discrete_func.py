#!/usr/bin/env python

def discrete_func(f, a, b, n):
    x = linspace(a, b, n+1)
    y = zeros(len(x))
    for i in xrange(len(x)):
        y[i] = func(x[i])
    return x, y

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

x, y = discrete_func(f, a, b, n)
plot(x, y)
