# example from Pang, Ch. 4
#
# Solve a Poisson equation via shooting
#
# u'' = -0.25*pi**2 (u + 1)
#
# with u(0) = 0, u(1) = 1
#
# this has the analytic solution: u(x) = cos(pi x/2) + 2 sin(pi x/2) - 1
#
# M. Zingale

from __future__ import print_function

import numpy as np
import math
import matplotlib.pyplot as plt

def rk4(y1_0, eta, rhs, xl=0.0, xr=1.0, n=100):
    """
    R-K 4 integration: y1_0 and eta are y1(0) and y2(0) rhs is the
    righthand side function xl and xr are the domain limits n
    is the number of integration points (including starting point)

    """
    h = (xr - xl)/(n-1)

    y1 = np.zeros(n)
    y2 = np.zeros(n)

    # left boundary initialization
    y1[0] = y1_0
    y2[0] = eta

    for m in range(n-1):
        dy1dx_1, dy2dx_1 = rhs(y1[m], y2[m])
        dy1dx_2, dy2dx_2 = rhs(y1[m] + 0.5*h*dy1dx_1, y2[m] + 0.5*h*dy2dx_1)
        dy1dx_3, dy2dx_3 = rhs(y1[m] + 0.5*h*dy1dx_2, y2[m] + 0.5*h*dy2dx_2)
        dy1dx_4, dy2dx_4 = rhs(y1[m] + h*dy1dx_3, y2[m] + h*dy2dx_3)

        y1[m+1] = y1[m] + (h/6.0)*(dy1dx_1 + 2.0*dy1dx_2 + 2.0*dy1dx_3 + dy1dx_4)
        y2[m+1] = y2[m] + (h/6.0)*(dy2dx_1 + 2.0*dy2dx_2 + 2.0*dy2dx_3 + dy2dx_4)

    return y1, y2


def rhs(y1, y2):
    """ RHS function.  Here y1 = u, y2 = u'
        This means that our original system is:
           y2' = u'' = -0.25*pi**2 (u+1) """

    dy1dx = y2
    dy2dx = -0.25*math.pi**2 * (y1 + 1.0)

    return dy1dx, dy2dx


def analytic(x):
    """ analytic solution """
    return np.cos(math.pi*x/2) + 2.0*np.sin(math.pi*x/2) - 1.0


# shoot from x = 0 to x = 1.  We will do this by selecting a boundary
# value for y2 and use a secant method to adjust it until we reach the
# desired boundary condition at y1(1)
x_left = 0.0
x_right = 1.0

# desired right BC, y1(1)
y1_right_true = 1.0

# number of integration points
npts = 32

# desired tolerance
eps = 1.e-8

# initial guess
y1_0 = 0.0   # this is the correct boundary condition a x = 0
eta  = 0.0   # this is what we will adjust to get the desired y1(1)


# integrate
y1_old, y2_old = rk4(y1_0, eta, rhs, xl=0.0, xr=1.0, n=npts)

x = np.linspace(0.0, 1.0, npts)
plt.scatter(x, y1_old, label="initial guess", marker="x")

# new guess -- we don't have any info on how to compute this yet, so
# just choose something
eta_m1 = eta   # store the old guess
eta = -1.0

# Secant loop
dy = 1000.0    # fail first time through

# keep track of iteration for plotting
iter = 1

while dy > eps:

    # integrate
    y1, y2 = rk4(y1_0, eta, rhs, xl = 0.0, xr = 1.0, n=npts)

    plt.scatter(x, y1, label="iteration {}".format(iter), marker="x")

    # do a Secant method to correct.  Here eta = y2(0) -- our
    # control parameter.  We want to zero:
    #   f(eta) = y1_true(1) - y1^(eta)(1)

    # derivative (for Secant)
    dfdeta = ( (y1_right_true - y1_old[npts-1]) -
               (y1_right_true - y1[npts-1]) ) / (eta_m1 - eta)

    # correction by f(eta) = 0 = f(eta_0) + dfdeta deta
    deta = -(y1_right_true - y1[npts-1])/dfdeta

    # store the old guess and correct
    eta_m1 = eta
    eta += deta

    dy = abs(deta)

    y1_old = y1
    y2_old = y2

    iter += 1


print("final slope, eta = ", eta)

plt.plot(x, analytic(x), color="0.5", label="analytic")

plt.xlabel("x")
plt.ylabel(r"$y_1$")

leg = plt.legend(frameon=False, fontsize="small")

plt.xlim(0.0, 1.0)

plt.tight_layout()
plt.savefig("shoot.png", dpi=150)
