# fit to y(x) = a_1 exp(a2 x) using a nonlinear fitting technique that
# reduces to a multivariate root finding problem
#
# This is very sensitive to our initial guess
#
# M. Zingale (2013-03-10)

import numpy as np
import numpy.linalg
import matplotlib.pyplot as plt


tol = 1.e-5

def fun(a, x, y):
    """ the derivatives of our fitting function wrt each parameter:

        Q = sum_{i=1}^N (y_i = a0 exp(a1 x_i) )**2

        -- these are what we zero """

    # dQ/da0
    f0 = np.sum(np.exp(a[1]*x)*(a[0]*np.exp(a[1]*x) - y))

    # dQ/da1
    f1 = np.sum(x*np.exp(a[1]*x)*(a[0]*np.exp(a[1]*x) - y))

    return np.array([f0, f1])


def jac(a, x, y):
    """ return the Jacobian of fun """

    # df0/da0
    df0da0 = np.sum(np.exp(2.0*a[1]*x))

    # df0/da1
    df0da1 = np.sum(x*np.exp(a[1]*x)*(2.0*a[0]*np.exp(a[1]*x) - y))

    # df1/da0
    df1da0 = np.sum(x*np.exp(2.0*a[1]*x))

    # df1/da1
    df1da1 = np.sum(x**2*np.exp(a[1]*x)*(2.0*a[0]*np.exp(a[1]*x) - y))

    return np.array([ [df0da0, df0da1], [df1da0, df1da1] ])



def fRoots(aguess, x, y):
    """ aguess is the initial guess to our fit parameters.  x and y
        are the vector of points that we are fitting to """

    avec = aguess.copy()

    err = 1.e100
    while err > tol:

        # get the jacobian
        J = jac(avec, x, y)

        print("condition number of J: ", np.linalg.cond(J))

        # get the current function values
        f = fun(avec, x, y)

        # solve for the correction: J dx = -f
        da = np.linalg.solve(J, -f)

        avec += da
        err = np.max(np.abs(da))

    return avec



# make up some experimental data
a0 = 2.5
a1 = 2./3.
sigma = 2.0

x = np.linspace(0.0, 4.0, 25)
y = a0*np.exp(a1*x) + sigma*np.random.randn(len(x))

plt.scatter(x,y)
plt.errorbar(x, y, yerr=sigma, fmt=None, label="_nolegend_")

# initial guesses
aguess = [1.0, 1.0]

# fit
afit = fRoots(aguess, x, y)

print(afit)

p = plt.plot(x, afit[0]*np.exp(afit[1]*x),
             label=r"$a_0 = $ %f; $a_1 = $ %f" % (afit[0], afit[1]))

plt.legend(numpoints=1, frameon=False)

plt.savefig("nonlinear-fit.png")
