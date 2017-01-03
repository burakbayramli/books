#!/usr/bin/env python

# There are three basic styles of using Matplotlib, each one is
# exemplified below.

# Style 1: pylab (Matlab-like)
# import matplotlib's plotting functions and 
# do an almost from numpy import * in addition:
from matplotlib.pylab import *  
t = linspace(0, 3, 51)    # 51 points between 0 and 3
y1 = t**2*exp(-t**2)
plot(t, y1)

y2 = t**4*exp(-t**2)
# pick out each 4 points and add random noise:
t3 = t[::4]

seed(11)
y3 = y2[::4] + 0.02*randn(t3.size)

hold(True)
plot(t,  y2, 'b-')
plot(t3, y3, 'bo')
legend(['y', 'y2', 'y3'])
xlabel('t')
ylabel('y')
title('Simple Plot Demo with matplotlib.pylab')
savefig('tmp1.eps')
#show() # prevents the next from being shown, place at end of script only

# Style 2: matplotlib.pyplot
import matplotlib.pyplot as plt
import numpy as np
t = linspace(0, 3, 51)    # 51 points between 0 and 3
y1 = t**2*exp(-t**2)
y2 = t**4*exp(-t**2)
t3 = t[::4]
np.random.seed(11)
y3 = y2[::4] + np.random.normal(loc=0, scale=0.02, size=t3.size)
plt.figure()  # new figure
plt.plot(t,  y1, 'r-', label='y1')
plt.plot(t,  y2, 'b-', label='y2')
plt.plot(t3, y3, 'bo', label='y3')
plt.legend(loc='upper right')
plt.xlabel('t')
plt.ylabel('y')
plt.title('Simple Plot Demo with matplotlib.pyplot')
plt.savefig('tmp2.png')
#plt.show()

# Style 3: matplotlib OO
fig = plt.figure()  # create Figure object
# or
#from matplotlib.figure import Figure
#fig = Figure()
ax = fig.add_subplot(1, 1, 1)  # create Axes object
# create Line2D objects for each line in the plot:
y1_plot, = ax.plot(t,  y1, 'r-', label='y1')
y2_plot, = ax.plot(t,  y2, 'b-', label='y2')
y3_plot, = ax.plot(t3, y3, 'bo', label='y3')
ax.set_xlabel('t')
ax.set_ylabel('y')
ax.set_title('Simple Plot Demo with matplotlib OO')
ax.legend()
fig.savefig('tmp3.png')

plt.show()

