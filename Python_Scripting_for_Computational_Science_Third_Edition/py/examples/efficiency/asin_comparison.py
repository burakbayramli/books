#!/usr/bin/env python
"""
Comparison of scalar and vectorized sine functions when
the argument is a scalar.
A test on a**b vs pow(a,b) is also included.
"""

import timeit, sys

try:
    n = int(sys.argv[1])
except:
    n = 1000000  # default

print 'n =', n
from scitools.EfficiencyTable import EfficiencyTable

e = EfficiencyTable('%d evaluations of arcsin(0.4)' % n)

t1 = timeit.Timer('arcsin(0.4)',
                  setup='from numpy import arcsin').timeit(n)
t2 = timeit.Timer('asin(0.4)',
                  setup='from math import asin').timeit(n)
t3 = timeit.Timer('arcsin(0.4)',
                  setup='from numarray import arcsin').timeit(n)
t4 = timeit.Timer('arcsin(0.4)',
                  setup='from Numeric import arcsin').timeit(n)
t5 = timeit.Timer('arcsin(0.4)',
                  setup='from numpy.lib.scimath import arcsin').timeit(n)
e.add('numpy arcsin', t1)
e.add('math asin', t2)
e.add('numarray arcsin', t3)
e.add('Numeric arcsin', t4)
e.add('numeric.lib.scimath arcsin', t5)
print e

# call to function involving intrinsic functions:
e = EfficiencyTable('calling arcsin, arccos inside a function')
sc = 'myfunc(0.4, 0.4)'
f = """ arcsin, arccos
def myfunc(x, y):
    return x**2 + arccos(x*y)*arcsin(x)
"""
f2 = """ asin, acos
def myfunc(x, y):
    return x**2 + acos(x*y)*asin(x)
"""
t7 = timeit.Timer(sc, setup='from numpy import'+f).timeit(n)
t8 = timeit.Timer(sc, setup='from math import'+f2).timeit(n)
t9 = timeit.Timer(sc, setup='from numpy.lib.scimath import'+f).timeit(n)
e.add('arcsin from numpy', t7)
e.add('asin from math', t8)
e.add('arcsin from numpy.lib.scimath', t9)
f = """
def myfunc(x, y):
    return x**2 + N.arccos(x*y)*N.arcsin(x)
"""
f2 = """
def myfunc(x, y):
    return x**2 + N.acos(x*y)*N.asin(x)
"""
t7 = timeit.Timer(sc, setup='import numpy as N' + f).timeit(n)
t8 = timeit.Timer(sc, setup='import math as N'+f2).timeit(n)
t9 = timeit.Timer(sc, setup='import numpy.lib.scimath as N'+f).timeit(n)
e.add('numpy.arcsin', t7)
e.add('math.arcsin', t8)
e.add('numpy.lib.scimath.arcsin', t9)
print e
