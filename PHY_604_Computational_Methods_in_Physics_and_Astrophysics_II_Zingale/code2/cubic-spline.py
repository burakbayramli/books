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
import math
from scipy import linalg
from scipy import interpolate


# plot a spline
def plot_spline(x0, x1, f0, f1, ppp0, ppp1):
    
    # lots of points for a smooth plot
    x = np.linspace(x0, x1, 100)

    dx = x1-x0

    alpha = ppp1/(6.0*dx)
    beta = -ppp0/(6.0*dx)

    gamma = (-ppp1*dx*dx/6.0 + f1)/dx
    eta = (ppp0*dx*dx/6.0 - f0)/dx

    p = alpha*(x-x0)**3 + beta*(x-x1)**3 + gamma*(x-x0) + eta*(x-x1)

    plt.plot(x, p)


# number of intervals
n = 20

xmin = 0.0
xmax = 1.0


# coordinates of the data locations
x = np.linspace(xmin, xmax, n+1)
dx = x[1] - x[0]

# random data
f = np.random.rand(n+1)


# we are solving for n-1 unknowns

# setup the righthand side of our matrix equation
b = np.zeros(n+1)

# b_i = (6/dx) * (f_{i-1} - 2 f_i + f_{i+1})
# here we do this with slice notation to fill the
# inner n-1 slots of b
b[1:n] = (6.0/dx)*(f[0:n-1] - 2.0*f[1:n] + f[2:n+1])

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

d[:] = 4.0*dx

u[:] = dx
u[0] = 0.0

l[:] = dx
l[n-2] = 0.0

# create a banded matrix -- this doesn't store every element -- just
# the diagonal and one above and below
A = np.matrix([u,d,l])

# solve Ax = b using the scipy banded solver -- the (1,1) here means
# that there is one diagonal above the main diagonal, and one below.
xsol = linalg.solve_banded((1,1), A, b)

# x now hold all the second derivatives for points 1 to n-1.  Natural
# boundary conditions set p'' = 0 at i = 0 and n
# ppp will be our array of second derivatives
ppp = np.insert(xsol, 0, 0)  # insert before the first element
ppp = np.insert(ppp, n, 0)   # insert at the end


# now plot -- data points first
plt.scatter(x, f, marker="x", color="r")

# plot the splines
for i in range(n):

    # working on interval [i,i+1]
    ppp_i = ppp[i]
    ppp_ip1 = ppp[i+1]

    f_i = f[i]
    f_ip1 = f[i+1]

    x_i = x[i]
    x_ip1 = x[i+1]

    plot_spline(x_i, x_ip1, f_i, f_ip1, ppp_i, ppp_ip1)

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





