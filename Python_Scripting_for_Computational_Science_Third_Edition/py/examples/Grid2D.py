#!/usr/bin/env python

from numpy import *
from scitools.numpyutils import seq
import time, sys

class Grid2D:
    def __init__(self,
                 xmin=0, xmax=1, dx=0.5,
                 ymin=0, ymax=1, dy=0.5):
        # coordinates in each space direction:
        self.xcoor = seq(xmin, xmax, dx)
        self.ycoor = seq(ymin, ymax, dy)

        # store for convenience:
        self.dx = dx;  self.dy = dy  
        self.nx = self.xcoor.size;  self.ny = self.ycoor.size

        # make two-dim. versions of the coordinate arrays:
        # (needed for vectorized function evaluations)
        self.xcoorv = self.xcoor[:, newaxis]
        self.ycoorv = self.ycoor[newaxis, :]

    def vectorized_eval(self, f):
        """Evaluate a vectorized function f at each grid point."""
        return f(self.xcoorv, self.ycoorv)
    
    def __call__(self, f):
        a = f(self.xcoorv, self.ycoorv)
        return a

    def __str__(self):
        s = '\nx: ' + str(self.xcoor) + '\ny: ' + str(self.ycoor)
        return s

    def gridloop(self, f):
        """
        Implement strights loops as a simple (and slow)
        alternative to the vectorized code in __call__.
        """
        a = zeros((self.nx, self.ny))
        for i in xrange(self.nx):
            x = self.xcoor[i]
            for j in xrange(self.ny):
                y = self.ycoor[j]
                a[i,j] = f(x, y)
        return a

    def gridloop_hardcoded_func(self):
        """As gridloop, but hardcode a function formula."""
        a = zeros((self.nx, self.ny))
        for i in xrange(self.nx):
            x = self.xcoor[i]
            for j in xrange(self.ny):
                y = self.ycoor[j]
                a[i,j] = sin(x*y) + 8*x
        return a

    def gridloop_list(self, f):
        """As gridloop, but use a list instead of a NumPy array."""
        a = []
        for i in xrange(self.nx):
            a.append([])
            x = self.xcoor[i]
            for j in xrange(self.ny):
                y = self.ycoor[j]
                a_value = f(x, y)
                a[i].append(a_value)
        return a

    def gridloop_itemset(self, f):
        """
        Implement strights loops, but use numpy's itemset
        method.
        """
        a = zeros((self.nx, self.ny))
        for i in xrange(self.nx):
            x = self.xcoor.item(i)
            for j in xrange(self.ny):
                y = self.ycoor.item(i)
                a.itemset(i,j, f(x, y))
        return a


def plot_easyviz(grid, func_values):
    from scitools.easyviz import mesh
    mesh(grid.xcoorv, grid.ycoorv, func_values, indexing='ij')
    # indexing='ij' will become standard
    
def plot_Gnuplot(grid, func_values):
    import Gnuplot
    g = Gnuplot.Gnuplot(persist=1)
    g('set parametric')
    g('set data style lines')
    g('set hidden')
    g('set contour base')
    g.splot(Gnuplot.GridData(func_values, grid.xcoor, grid.ycoor))
    time.sleep(2)  # give Gnuplot some time to make the plot
    """
    More examples on plotting two-dimensional scalar fields in Gnuplot
    can be found in the demo.py script that comes with the
    Python package containing the Gnuplot module.
    """
        

def timing1(n=2000):
    # timing:
    dx = 1.0/n
    g = Grid2D(xmin=0, xmax=1, dx=dx,
               ymin=0, ymax=1, dy=dx)

    def myfunc(x, y):
        return sin(x*y) + 8*x

    from scitools.StringFunction import StringFunction
    expression = StringFunction('sin(x*y) + 8*x',
                                independent_variables=('x','y'),
                                globals=globals()  # needed for vectorization
                                )

    from scitools.misc import timer
    from scitools.EfficiencyTable import EfficiencyTable
    e = EfficiencyTable('Grid2D efficiency tests, %dx%d grid' % (n,n))
    t0 = time.clock()

    # vectorized expressions are so fast that we run the code
    # repeatedly
    rep=20
    t1 = timer(g.__call__, (expression,), repetitions=rep,
               comment='StringFunction')
    e.add('g.__call__, expression (vectorized)', t1)
    t2 = timer(g.__call__, (myfunc,), repetitions=rep, comment='myfunc')
    e.add('g.__call__, myfunc (vectorized)', t2)

    f = g.gridloop_hardcoded_func()
    t3 = timer(g.gridloop_hardcoded_func, (), repetitions=1, comment='')
    e.add('g.gridloop_hardcoded_func', t3)

    t4 = timer(g.gridloop, (expression,), repetitions=1,
               comment='StringFunction')
    e.add('g.gridloop, expression (explicit loops)', t4)
    t5 = timer(g.gridloop, (myfunc,), repetitions=1, comment='myfunc')
    e.add('g.gridloop, myfunc (explicit loops)', t4)

    t6 = timer(g.gridloop_list, (expression,), repetitions=1,
               comment='StringFunction')
    e.add('g.gridloop_list, expression (explicit loops)', t6)
    t7 = timer(g.gridloop_list, (myfunc,), repetitions=1, comment='myfunc')
    e.add('g.gridloop_list, myfunc (explicit loops)', t7)

    from math import sin as mathsin  # efficiency trick
    # The scalar computations above used sin from NumPy, which is
    # known to be slow for scalar arguments. Here we use math.sin
    # (stored in mathsin, could also use the slightly slower math.sin
    # explicitly)
    # taken globally so eval works: from math import sin as mathsin
    def myfunc_scalar(x, y):
        return mathsin(x*y) + 8*x
    expression_scalar = StringFunction('mathsin(x*y) + 8*x',
                                       independent_variables=('x','y'),
                                       globals=globals())
    t8 = timer(g.gridloop, (expression_scalar,), repetitions=1,
               comment='eval(str)')
    e.add('g.gridloop, expression_scalar', t8)
    t9 = timer(g.gridloop, (myfunc_scalar,), repetitions=1, comment='myfunc')
    e.add('g.gridloop, myfunc_scalar', t9)
    print e

def verify1():
    g = Grid2D()
    from scitools.StringFunction import StringFunction
    expression = StringFunction('x + a*y',
                                independent_variables=('x','y'),
                                globals=globals(), a=2)
    f = g(expression)
    print g, f
    f = g.gridloop(expression)
    print g, f
    g = Grid2D(dx=0.05, dy=0.025)
    f = g.vectorized_eval(expression)
    plot_easyviz(g, f)
    #plot_Gnuplot(g, f)
    
def _run():
    try:
        func = sys.argv[1]
    except:
        func = 'verify1'
    if func == 'timing1':
        try:
            n = int(sys.argv[2])
        except:
            n = 2000
        exec 'timing1(%d)' % n
    else:
        exec func + '()'
    
if __name__ == '__main__':
    _run()


