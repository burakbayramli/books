"""
Is x*x more efficient than x**2 for a large array x?

This program applies the timeit module to investigate this
question.
"""
import timeit
n = 1000000  # length of array

initcode = """
from numpy import zeros
from __main__ import n
x = zeros(n)
"""

nrep = 80  # repeat x*x and x**2 nrep times

t = timeit.Timer('x*x', setup=initcode)
t1 = t.timeit(nrep)
t = timeit.Timer('x**2', setup=initcode)
t2 = t.timeit(nrep)
print 'x**2 vs x*x for arrays of length %d:' % n, t2/t1

# Repeat the test for math.pow and ** for scalars

nrep = 8000000

t = timeit.Timer('2.0*2.0')
t1 = t.timeit(nrep)
t = timeit.Timer('2.0**2')
t2 = t.timeit(nrep)
print '2.0**2 vs 2.0*2.0:', t2/t1
t = timeit.Timer('2.0**2.0')
t2 = t.timeit(nrep)
print '2.0**2.0 vs 2.0*2.0:', t2/t1
t = timeit.Timer('pow(2.0,2)', setup='from math import pow')
t2 = t.timeit(nrep)
print 'pow(2.0,2) vs 2.0*2.0:', t2/t1

