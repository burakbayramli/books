#!/usr/bin/env python

def trapezoidal(f, a, b, n):
    h = (b-a)/float(n)
    I = f(a) + f(b)
    for k in xrange(1, n, 1):
        x = a + k*h
        I += 2*f(x)
    I *= h/2
    return I


from math import *
from scitools.StringFunction import StringFunction
import sys

def test(argv=sys.argv):
    try:
        f_formula = argv[1]
        a = eval(argv[2])
        b = eval(argv[3])
        n = int(argv[4])
    except:
        print "usage: %s 'f(x)' a b n" %sys.argv[0]
        sys.exit(1)

    f = StringFunction(f_formula)
    I = trapezoidal(f, a, b, n)
    print 'Approximation of the integral: ', I

if __name__ == '__main__':
    test()
