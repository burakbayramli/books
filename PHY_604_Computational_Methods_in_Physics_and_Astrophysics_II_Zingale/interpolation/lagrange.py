# lagrange interpolation example

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

def sine_test(x):
    """ a sine test function """
    return np.sin(x)

def tanh_test(x):
    """ a tanh test function """
    return 0.5*(1.0+np.tanh((x-1.0)/0.1))


class LagrangePoly(object):
    """ a general class for creating a Lagrange polynomial 
    representation of a function """

    def __init__(self, points="fixed", N=3, xmin=0.0, xmax=1.0, func=None):
        self.points = points
        self.N = N
        self.xmin = xmin
        self.xmax = xmax
        self.func = func

        self.xp = self.compute_interp_points()
        self.fp = func(self.xp)

    def compute_interp_points(self):
        """ get the x points that we interpolate at """
        if self.points == "fixed":
            x = np.linspace(self.xmin, self.xmax, self.N)

        elif self.points == "variable":
            # the Chebyshev nodes
            x = 0.5*(self.xmin + self.xmax) + \
                0.5*(self.xmax - self.xmin)*np.cos(2.0*np.arange(self.N)*np.pi/(2*self.N))

        return x

    def evalf(self, x):
        """ given a point x and a function func, fit a Lagrange polynomial through the
        control points and return the interpolated value at x """

        f = 0.0

        # sum over points
        for m in range(len(self.xp)):

            # create the Lagrange basis polynomial for point m
            l = None

            for n in range(len(self.xp)):
                if n == m:
                    continue

                if l is None:
                    l = (x - self.xp[n])/(self.xp[m] - self.xp[n])
                else:
                    l *= (x - self.xp[n])/(self.xp[m] - self.xp[n])

            f += self.fp[m]*l

        return f


def main():

    # globals to control some behavior
    func_type = "tanh"   # can be sine or tanh
    points = "fixed"  # can be variable or fixed

    max_npts = 19  # must be >= 3

    if func_type == "sine":
        xmin = 0.0
        xmax = 2.0*np.pi
        func = sine_test
    elif func_type == "tanh":
        xmin = 0.0
        xmax = 2.0
        func = tanh_test

    for npts in range(3, max_npts+1, 2):

        l = LagrangePoly(points, npts, xmin, xmax, func=func)

        # xx are the finely grided data that we will interpolate at to get
        # the interpolated function values ff
        xx = np.linspace(xmin, xmax, 200)
        ff = np.zeros_like(xx)

        for n, v in enumerate(xx):
            ff[n] = l.evalf(v)

        # exact function values at the interpolated points
        fexact = func(xx)

        # error
        e = fexact-ff

        plt.clf()
        plt.subplot(211)

        plt.scatter(l.xp, l.fp, marker="x", s=30)
        plt.plot(xx, ff)

        plt.plot(xx, fexact, color="0.5")

        plt.xlim(xmin, xmax)


        plt.subplot(212)

        plt.plot(xx, e)

        plt.xlim(xmin, xmax)

        f = plt.gcf()
        f.set_size_inches(7.0, 6.0)

        plt.suptitle("Lagrange interpolation of {} using n={} {} points".format(func_type, npts, points))

        plt.savefig("lagrange_{}_n{}.png".format(points, npts))

if __name__ == "__main__":
    main()
