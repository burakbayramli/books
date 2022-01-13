# a simple cubic spline example.
#
# generate some random data in 10 intervals -- note the data changes
# each time this is run.
#
# Our form of the spline polynomial comes from Pang, Ch. 2
#
# in this version, we use an iterative method to solve the tridiagonal system
#
# plot the splines
#
# M. Zingale

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
from scipy import linalg   # scipy modules need to be imported separately

# Jacobi tolerance
tol = 1.e-12

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

# use Jacobi iteration to solve this system.  Note, we don't need to
# write down the matrix for this, we know that the form of the system
# we are solving is:
#
#   dx x_{i-1} + 4 dx x_i + dx x_{i+1} = b_i
#
# we only solve for i = 1, ..., n-1, and we impose the boundary
# conditions: x[0] = x[n] = 0

# allocate space for the solution and old value
xold = np.zeros(n+1)
xsol = np.zeros(n+1)

# note that the BCs: x[0] = x[n] = 0 are already initialized

# iterate
err = 1.e30
iter = 0
while err > tol:
    # xsol_i = (b_i - dx x_{i-1} - dx x_{i+1})/(4dx)
    xsol[1:n] = (b[1:n] - dx*xold[0:n-1] - dx*xold[2:n+1])/(4.0*dx)

    iter += 1
    err = np.max(np.abs((xsol[1:n] - xold[1:n])/xsol[1:n]))

    print(iter, err)
    xold = xsol.copy()



# for debugging, use the built-in banded solver from numpy
u = np.zeros(n-1)
d = np.zeros(n-1)
l = np.zeros(n-1)

d[:] = 4.0*dx
u[:] = dx; u[0] = 0.0
l[:] = dx; l[n-2] = 0.0

# create a banded matrix -- this doesn't store every element -- just
# the diagonal and one above and below and solve
A = np.matrix([u,d,l])
xsol_np = linalg.solve_banded((1,1), A, b[1:n])

# xsol_np now hold all the second derivatives for points 1 to n-1.
# Natural boundary conditions are imposed here
xsol_np = np.insert(xsol_np, 0, 0)
xsol_np = np.insert(xsol_np, n, 0) # insert at the end

# report error from our iterative method vs. direct solve
err = np.max(np.abs((xsol[1:n] - xsol_np[1:n])/xsol_np[1:n]))

print("relative error of iterative solution compared to direct: ", err)

# go ahead with our iterative solution -- natural boundary conditions are
# already in place for this
ppp = xsol

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


plt.savefig("spline-iterative.png")


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
