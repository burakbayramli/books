from timeit import *

t = Timer('bisection(f, a, b, eps=1.0e-6)',
          setup="""
from nonlinear_solvers import bisection
def f(x):
    return x**2 - 9

a = 0;   b = 1000
""")
no_runs = 100000
print "CPU time (%d runs): %f" % (no_runs, t.timeit(no_runs))

