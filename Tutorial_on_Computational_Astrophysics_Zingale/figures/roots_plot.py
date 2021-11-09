"""
make plots using the root finding methods in roots.py

M. Zingale (2013-02-14)
"""

from __future__ import print_function

import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes, mark_inset

mpl.rcParams['xtick.labelsize'] = 14
mpl.rcParams['ytick.labelsize'] = 14
mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'

# sample equations to find the roots of
def f(x):
    return 0.5*x**3 + np.pi/3.0 * x + 2

def fprime(x):
    return 1.5*x**2 + np.pi/3.0


class Root(object):
    """ simple class to manage root finding.  All method take in a
        function and desired tolerance """

    def __init__(self, fun, tol=1.e-10, fprime=None):
        self.f = fun
        self.fprime = fprime

        self.tol = tol

    def newton(self, x0):
        """ find the root via Newton's method.  x0 is the initial guess
            for the root """

        xeval = []

        # initial change
        dx = -self.f(x0)/self.fprime(x0)
        xeval.append(x0)
        x = x0 + dx

        while abs(dx) > self.tol:
            dx = -self.f(x)/self.fprime(x)
            xeval.append(x)
            x += dx

        return x, xeval

r = Root(f, fprime=fprime)

xmin = -3.5
xmax = 3.5

xfine = np.linspace(xmin, xmax, 200)

#---------------------------------------------------------------------------
# plot Newton

x0 = 3.0
root, xeval = r.newton(x0)

print(xeval)

for n, x in enumerate(xeval[:-1]):
    plt.clf()
    plt.plot(xfine, r.f(xfine))

    plt.scatter(np.array(xeval[0:n+1]),
                r.f(np.array(xeval[0:n+1])),
                marker="o", s=25, color="r", zorder=100)

    plt.plot(xfine, r.fprime(x)*(xfine-x) + r.f(x), color="0.5")

    xintercept = -r.f(x)/r.fprime(x) + x

    plt.scatter([xintercept], [0], marker="x", s=25, color="r", zorder=100)

    if n%2 == 0:
        plt.text(x, r.f(x)-0.3, "{}".format(n),
                 color="r", fontsize="16",
                 verticalalignment="top", horizontalalignment="center", zorder=1000)
    else:
        plt.text(x, r.f(x)+0.3, "{}".format(n),
                 color="r", fontsize="16",
                 verticalalignment="bottom", horizontalalignment="center", zorder=1000)

    F = plt.gcf()

    plt.text(0.4, 0.08, f"iteration: {n}",
             transform = F.transFigure, fontsize="12", color="r")

    plt.text(0.4, 0.05, f"initial guess = {x}",
             transform = F.transFigure, fontsize="12", color="r")

    plt.text(0.4, 0.02, f"new guess = {xeval[n+1]}",
             transform = F.transFigure, fontsize="12", color="r")

    # axes through origin
    ax = plt.gca()
    ax.spines['left'].set_position('zero')
    ax.spines['right'].set_color('none')
    ax.spines['bottom'].set_position('zero')
    ax.spines['top'].set_color('none')
    #ax.spines['left'].set_smart_bounds(True)
    #ax.spines['bottom'].set_smart_bounds(True)
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')

    plt.xlim(xmin, 1.1*xmax)
    plt.ylim(-10.0,20)


    f = plt.gcf()
    f.set_size_inches(7.20,7.20)


    plt.tight_layout()

    plt.savefig(f"newton_{n:02d}.png")
