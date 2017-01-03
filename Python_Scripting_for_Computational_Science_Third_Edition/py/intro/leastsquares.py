#!/usr/bin/env python
"""Demonstrate fitting a stright line to data with NumPy."""
n = 20                      # no of data points
from numpy import *

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

# alternative method: numpy.polyfit
a, b = polyfit(x, y, 1)

headline = 'fitted y = %.2f*x + %.2f:' % (a, b)


# import matplotlib's Matlab-like plotting functions and 
# do a from numpy import * in addition:
from matplotlib.pylab import *  
#figure()
#subplot(3, 1, 1)
plot(x, y,'o')
hold(True)
plot(x, y_line, 'r')
plot(x, a*x + b, 'b')
legend(['data points', 'original line', 'fitted line'])
xlabel('input')
ylabel('response')
title('matplotlib pylab: ' + headline)
savefig('tmp3.eps')
#show() # prevents the next from being shown

# matplotlib.pyplot style:
import matplotlib.pyplot as plt
plt.plot(x, y, 'o', label='data points')
plt.plot(x, y_line, 'r', label='original line')
plt.plot(x, a*x + b, 'b', label='fitted line')
plt.legend(loc='lower left')
plt.xlabel('input')
plt.ylabel('response')
plt.title('matplotlib.pyplot: ' + headline)
plt.savefig('tmp4.png')
#plt.show()

# matplotlib OO style:
fig = plt.figure()  # create Figure object
# or
#from matplotlib.figure import Figure
#fig = Figure()
ax = fig.add_subplot(1, 1, 1)  # create Axes object
# create Line2D objects for each line in the plot:
data_points, = ax.plot(x, y, 'o', label='data points')
orig_line,   = ax.plot(x, y_line, 'r', label='original line')
fitted_line, = ax.plot(x, a*x + b, 'b', label='fitted line')
ax.set_xlabel('input')
ax.set_ylabel('response')
ax.set_title('matplotlib OO: ' + headline)
ax.legend()
fig.savefig('tmp5.png')

plt.show()
# kill Matplotlib windows before proceeding

# plot with scitools.easyviz:
from scitools.easyviz import *  
# or from scitools.std import *
subplot(2, 1, 1)
plot(x, y,'o')
hold(True)
plot(x, y_line, 'r')
plot(x, a*x + b, 'b')
legend('data points', 'original line', 'fitted line')
xlabel('input data')
ylabel('response')
title('easyviz v1: ' + headline)
show()
hardcopy('tmp1.png')

subplot(2, 1, 2)
plot(x, y, 'o',
     x, y_line, 'r',
     x, a*x + b, 'b',
     legend=('data points', 'original line', 'fitted line'),
     xlabel='input',
     ylabel='response',
     title='easyviz v2: ' + headline,
     hardcopy='tmp2.ps')
