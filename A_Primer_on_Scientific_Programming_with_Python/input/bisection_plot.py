"""
Graphical illustration of the bisection algorithm for solving nonlinear
algebraic equations of the form f(x)=0.

Usage:
python bisection_plot.py f_formula a b [epsilon]

Note that f(x) must change sign in the interval [a,b].
"""
from bisection import bisection_evolution

import sys, time
usage = '%s f-formula a b [epsilon]' % sys.argv[0]
try:
    f_formula = sys.argv[1]
    a = float(sys.argv[2])
    b = float(sys.argv[3])
except IndexError:
    print usage; sys.exit(1)

try:  # is epsilon given on the command-line?
    epsilon = float(sys.argv[4])
except IndexError:
    epsilon = 1E-6  # default value

# Clean up all plot files
import glob, os
for filename in glob.glob('tmp_*.eps'): os.remove(filename)

from scitools.StringFunction import StringFunction
from scitools.std import *  # might be needed for f_formula
f = StringFunction(f_formula)
f.vectorize(globals())

results = bisection_evolution(f, a, b, epsilon)
if results is None:
    print 'f does not change sign in [%g, %g]' % (a, b)
    sys.exit(1)

# Visualize
x = linspace(a, b, 501)
y = f(x)
ymin = min(y);  ymax = max(y)
itcount = 1
for interval, m in results:
    a, b = interval
    plot(x, y, 'r-',
         [a, a], [ymin, ymax], 'g-',
         [b, b], [ymin, ymax], 'g-',
         [m, m], [ymin, ymax], 'b-',
         [x[0], x[-1]], [0, 0], 'y-',
         legend=('f(x)', 'a', 'b', 'm', 'y=0'),
         title='The Bisection method, iteration %d: [%.2g, %.2g]' % \
         (itcount, a, b), hardcopy='tmp_%02d.eps' % itcount)
    itcount += 1
    time.sleep(2)
    #raw_input('Type CR: ')

movie('tmp_*.eps', encoder='convert', fps=2,
      output_file='tmpmovie.gif')
