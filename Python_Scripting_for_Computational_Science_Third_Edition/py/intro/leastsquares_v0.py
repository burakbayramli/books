#!/usr/bin/env python
"""Demonstrate fitting a stright line to data with NumPy."""
# generate random data and fit a straight line

import sys
try:
    n = int(sys.argv[1])   # no of data points
except:
    n = 20

from scitools.std import *  # import numpy and much of scitools

# compute data points in x and y arrays:
# x in (0,1) and y=-2*x+3+eps, where eps is normally
# distributed with mean zero and st.dev. 0.25.
random.seed(20)
x = linspace(0.0, 1.0, n)
noise = random.normal(0, 0.25, n)
a_exact = -2.0; b_exact = 3.0
y_line = a_exact*x + b_exact
y = y_line + noise

# create least squares system:
A = array([x, zeros(n)+1])
A = A.transpose()
result = linalg.lstsq(A, y)
# result is a 4-tuple, the solution (a,b) is the 1st entry:
a, b = result[0]

# plot:
plot(x, y, 'o',
     x, y_line, 'r',
     x, a*x + b, 'b',
     legend=('data points', 'original line', 'fitted line'),
     title='y = %g*x + %g: fit to y = %g*x + %s + normal noise' % \
            (a, b, a_exact, b_exact),
     hardcopy='tmp.ps')
# make a PNG plot too:
hardcopy('tmp.png')

# alternative method: numpy.polyfit
a, b = polyfit(x, y, 1)
figure()
plot(x, y, 'o',
     x, y_line, 'r',
     x, a*x + b, 'b',
     legend=('data points', 'original line', 'fitted line'),
     title='y = %g*x + %g: polyfit(x,y,1) to y = %g*x + %s + normal noise' % \
            (a, b, a_exact, b_exact),
     hardcopy='tmp2.ps')


