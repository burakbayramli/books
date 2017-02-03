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
print t2/t1

