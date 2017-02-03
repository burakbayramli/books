#!/usr/bin/env python

def integrate(T, n, u0):
    h = T/float(n)
    t = linspace(0, T, n+1)
    I = f(t[0])
    for k in iseq(1, n-1, 1):
        I += 2*f(t[k])
    I += f(t[-1])
    I *= (h/2)
    I += u0
    return I

from scitools.std import *
try:
    f_formula = sys.argv[1]
    T  = eval(sys.argv[2])
    u0 = eval(sys.argv[3])
    n  = int(sys.argv[4])
except:
    print "usage: %s 'f(t)' t u0 n" % sys.argv[0]; sys.exit(1)
    
f = StringFunction(f_formula, independent_variables='t')
print "Numerical solution of u'(t)=t**3: ", integrate(T, n, u0)
