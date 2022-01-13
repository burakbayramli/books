# a simple cubic spline example.
#
# generate some random data in 10 intervals -- note the data changes
# each time this is run.
#
# Our form of the spline polynomial comes from Pang, Ch. 2
#
# solve the matrix system for the splines
#
# plot the splines
#

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

from scipy import linalg
from scipy import interpolate


class SplineInterp(object):
    """ do some spline interpolation given points (x, f).  We assume that the
    dx is constant """

    def __init__(self, x, f):
        self.x = x
        self.f = f
        self.dx = x[1] - x[0]

        self.ppp = self.solve_for_coeffs()


    def solve_for_coeffs(self):

        n = len(self.x)-1

        # we are solving for n-1 unknowns (e.g., neglecting the two
        # end points)

        # setup the righthand side of our matrix equation
        b = np.zeros(n+1)

        # b_i = (6/dx) * (f_{i-1} - 2 f_i + f_{i+1})
        # here we do this with slice notation to fill the
        # inner n-1 slots of b
        b[1:n] = (6.0/self.dx)*(self.f[0:n-1] - 2.0*self.f[1:n] + self.f[2:n+1])

        # we only care about the inner n-1 quantities
        b = b[1:n]


        # the matrix A is tridiagonal.  Create 3 arrays which will represent
        # the diagonal (d), the upper diagonal (u), and the lower diagnonal
        # (l).  l and u will have 1 less element.  For u, we will pad this at
        # the beginning and for l we will pad at the end.
        #
        # see http://docs.scipy.org/doc/scipy/reference/generated/scipy.linalg.solve_banded.html#scipy.linalg.solve_banded
        # for the description of a banded matrix

        u = np.zeros(n-1)
        d = np.zeros(n-1)
        l = np.zeros(n-1)

        d[:] = 4.0*self.dx

        u[:] = self.dx
        u[0] = 0.0

        l[:] = self.dx
        l[n-2] = 0.0

        # create a banded matrix -- this doesn't store every element -- just
        # the diagonal and one above and below
        A = np.matrix([u, d, l])

        # solve Ax = b using the scipy banded solver -- the (1,1) here means
        # that there is one diagonal above the main diagonal, and one below.
        xsol = linalg.solve_banded((1, 1), A, b)

        # x now hold all the second derivatives for points 1 to n-1.  Natural
        # boundary conditions set p'' = 0 at i = 0 and n
        # ppp will be our array of second derivatives
        ppp = np.insert(xsol, 0, 0)  # insert before the first element
        ppp = np.insert(ppp, n, 0)   # insert at the end

        return ppp


    def plot_spline(self, i, label=None, color="r"):
        """ plot the spline between [i, i+1] """

        x0 = self.x[i]
        x1 = self.x[i+1]

        # lots of points for a smooth plot
        npts = 100
        x = np.linspace(x0, x1, npts)

        f0 = self.f[i]
        f1 = self.f[i+1]

        dx = x1 - x0

        alpha = self.ppp[i+1]/(6.0*dx)
        beta = -self.ppp[i]/(6.0*dx)

        gamma = (-self.ppp[i+1]*self.dx**2/6.0 + f1)/self.dx
        eta = (self.ppp[i]*dx**2/6.0 - f0)/self.dx

        p = alpha*(x-x0)**3 + beta*(x-x1)**3 + gamma*(x-x0) + eta*(x-x1)

        plt.plot(x, p, color=color)

        if not label is None:
            plt.text(x[npts//2], p[npts//2]-0.025, label, color=color, fontsize=16)



def main():

    # number of intervals
    n = 20

    xmin = 0.0
    xmax = 1.0

    # coordinates of the data locations
    x = np.linspace(xmin, xmax, n+1)

    # random data
    f = np.random.rand(n+1)

    sp = SplineInterp(x, f)

    # now plot -- data points first
    plt.scatter(x, f, marker="x", color="r")

    # plot the splines
    colors = ["C0", "C1", "C2", "C3", "C4"]

    for i in range(n):
        ic = i % len(colors)
        sp.plot_spline(i, color=colors[ic])

    plt.savefig("spline.png")


# note: we could have done this all through scipy -- here is their
# spline, but it doesn't seem to support natural boundary conditions

#s = interpolate.InterpolatedUnivariateSpline(x, f, k=3)
#xx = np.linspace(xmin, xmax, 1000)
#plt.plot(xx, s(xx), color="k", ls=":")


# old way from scipy -- this raises a NotImplementedError for natural
#spl1 = interpolate.splmake(x, f, order=3, kind="natural")
#xx = np.linspace(xmin, xmax, 1000)
#yy = interpolate.spleval(spl1, xx)
#plt.plot(xx, yy, color="k", ls=":")

#plt.savefig("spline-scipy.png")



if __name__ == "__main__":
    main()
