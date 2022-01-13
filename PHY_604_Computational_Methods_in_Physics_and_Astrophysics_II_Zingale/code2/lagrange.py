# lagrange interpolation example

from __future__ import print_function

import math
import numpy as np
import matplotlib.pyplot as plt

# globals to control some behavior
func_type = "tanh"   # can be sine or tanh
points = "variable"  # can be variable or fixed

npts = 15

def fun_exact(x):
    """ the exact function that we sample to get the points to
    interpolate through """

    if func_type == "sine":
        return np.sin(x)
    elif func_type == "tanh":
        return 0.5*(1.0+np.tanh((x-1.0)/0.1))


def get_interp_points(N, xmin, xmax):
    """ get the x points that we interpolate at """
    if points == "fixed":
        x = np.linspace(xmin, xmax, N)

    elif points == "variable":
        # the Chebyshev nodes
        x = 0.5*(xmin + xmax) + \
            0.5*(xmax - xmin)*np.cos(2.0*np.arange(N)*math.pi/(2*N))

    return x
    
    
def lagrange_poly(x, xp, fp):
    """ given points (xp, fp), fit a lagrange polynomial and return
        the value at point x """

    f = 0.0
    
    # sum over points
    for m in range(len(xp)):

        # create the Lagrange basis polynomial for point m        
        l = None

        for n in range(len(xp)):
            if n == m:
                continue
            
            if l == None:
                l = (x - xp[n])/(xp[m] - xp[n])
            else:
                l *= (x - xp[n])/(xp[m] - xp[n])
                
        f += fp[m]*l

    return f


if func_type == "sine":
    xmin = 0.0
    xmax = 2.0*math.pi
elif func_type == "tanh":
    xmin = 0.0
    xmax = 2.0




# xp, fp are the points that we build the interpolant from
xp = get_interp_points(npts, xmin, xmax)
fp = fun_exact(xp)


# xx are the finely grided data that we will interpolate at to get
# the interpolated function values ff
xx = np.linspace(xmin, xmax, 200)
ff = np.zeros(len(xx))

for n in range(len(xx)):
    ff[n] = lagrange_poly(xx[n], xp, fp)

# exact function values at the interpolated points
fexact = fun_exact(xx)


# error
e = fexact-ff


plt.subplot(211)

plt.scatter(xp, fp, marker="x", color="r", s=30)
plt.plot(xx, ff, color="k")

plt.plot(xx, fexact, color="0.5")

plt.xlim(xmin, xmax)


plt.subplot(212)

plt.plot(xx, e)

plt.xlim(xmin, xmax)


plt.savefig("lagrange.png")

