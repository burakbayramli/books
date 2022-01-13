# compare various approximations of derivatives
from __future__ import print_function

import math
import numpy as np
import matplotlib.pyplot as plt

def func(x):
    """ the function to be plotted """
    f = np.sin(x)
    return f

def fprime(x):
    """ the analytic derivative of func(x) """
    fp = np.cos(x)
    return fp

def d1l(dx, fc, i):
    """ first-order, left-sided derivative at index i """
    D = (fc[i] - fc[i-1])/dx
    return D

def d1r(dx, fc, i):
    """ first-order, right-sided derivative at index i """
    D = (fc[i+1] - fc[i])/dx
    return D

def d2(dx, fc, i):
    """ second-order centered derivative at index i """
    D = (fc[i+1] - fc[i-1])/(2.0*dx)
    return D

def d4(dx, fc, i):
    """ fourth-order centered derivative at index i """
    D = -fc[i+2] + 8.0*fc[i+1] - 8.0*fc[i-1] + fc[i-2]
    D = D/(12.0*dx)
    return D


def line(x, slope, x0, y0):
    return y0 + slope*(x - x0)


xl = 0.0
xr = math.pi

# fine grid (to show exact function)
fine = np.linspace(xl, xr, 500)

# coarse grid (for differencing)
coarse = np.linspace(xl, xr, 10)

dx = coarse[1] - coarse[0]

# plot the fine gridded analytic function
f = func(fine)
plt.plot(fine, f, color="0.5", lw=2)



# plot the discrete data
c = func(coarse)
plt.scatter(coarse, c)


# get the derivative approximations
Dl = d1l(dx, c, 3)
Dr = d1r(dx, c, 3)
D2 = d2(dx, c, 3)
D4 = d4(dx, c, 3)

analytic = fprime(coarse[3])

# function and point values
x0 = coarse[3]
y0 = func(x0)

xplot = np.linspace(coarse[1], coarse[5], 50)

plt.plot(xplot, line(xplot, Dl, x0, y0), color="r", label="left-sided first-order approx")
plt.plot(xplot, line(xplot, Dr, x0, y0), color="b", label="right-sided first-order approx")
plt.plot(xplot, line(xplot, D2, x0, y0), color="g", label="centered second-order approx")
plt.plot(xplot, line(xplot, D4, x0, y0), color="k", label="centered fourth-order approx")
#plt.plot(xplot, line(xplot, analytic, x0, y0), color="0.5", ls=":", label="analytic")

print("analytic:          ", analytic)
print("left-sided O(dx):  ", Dl)
print("right-sided O(dx): ", Dr)
print("centered O(dx**2): ", D2)
print("centered O(dx**4): ", D4)

leg = plt.legend(loc=2,labelspacing=0.1)
ltext = leg.get_texts()
plt.setp(ltext, fontsize='small')
leg.draw_frame(0)

# axes run through 0
# http://matplotlib.org/examples/pylab_examples/spine_placement_demo.html
ax = plt.gca()
ax.spines['left'].set_position('zero')
ax.spines['right'].set_color('none')
ax.spines['bottom'].set_position('zero')
ax.spines['top'].set_color('none')
ax.spines['left'].set_smart_bounds(True)
ax.spines['bottom'].set_smart_bounds(True)
ax.xaxis.set_ticks_position('bottom')
ax.yaxis.set_ticks_position('left')

plt.xlim(0,2.0)

plt.savefig("fprime.png")


