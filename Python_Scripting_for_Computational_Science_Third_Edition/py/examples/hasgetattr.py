#!/usr/bin/env python
"""Examples of using setattr, getattr, and hasattr."""
from numpy import linspace
from scitools.misc import timer

def run(solvers, methods, data, datasets):
    results = {}
    # find largest data sets:
    maxsize = max([getattr(data, d).size for d in datasets \
                   if hasattr(data, d)])
    print maxsize
    # combine all solvers, methods, and datasets:
    for s in solvers:
        for m in methods:
            for d in datasets:
                if hasattr(solver, m) and hasattr(data, d):
                    f = getattr(solver, m)
                    x = getattr(data, d)
                    r = timer(f, (x,), repetitions=maxsize/x.size)
                    results[(m,d)] = r
    return results

class Extreme:
    """Find extreme values of NumPy data."""
    def __init__(self):
        pass

    def ravel(self, x):
        """Make x one-dimensional, use list min/max in Python."""
        rx = x.ravel()
        return min(rx), max(rx)

    def flat(self, x):
        """Use list min/max on x.flat."""
        return min(x.flat), max(x.flat)

    def native(self, x):
        """Use x.min(), x.max()."""
        return x.min(), x.max()

class Arrays:
    def __init__(self, n):
        self.d1 = linspace(1, n, n)
        self.d2 = linspace(1, n*n, n*n)
        self.d2.shape = (n, n)
        self.d3 = linspace(1, n*n*n, n*n*n)
        self.d3.shape = (n, n, n)
        self.maxsize = self.d3.size

data = Arrays(200)
solver = Extreme()
methods = ('ravel', 'flat', 'native')
datasets = ('d1', 'd2', 'd3', 'd4', 'd5')

r = run((solver,), methods, data, datasets)
for m, d in r:
    print '%s and %s : %g' % (m, d, r[(m,d)])
