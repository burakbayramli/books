# simple example of linear regression.  We make up some "experimental"
# data by calling a random number generator to perturb a line and then
# fit to that line
#
# M. Zingale

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

def y_experiment(a1, a2, sigma, x):
    """ return the experimental data in a linear + random fashion a1
        is the intercept, a2 is the slope, and sigma is the error """

    N = len(x)

    # randn gives samples from the "standard normal" distribution
    r = np.random.randn(N)

    y = a1 + a2*x + sigma*r

    return y


def y_experiment2(a1, a2, a3, sigma, x):
    """ return the experimental data in a quadratic + random fashion,
        with a1, a2, a3 the coefficients of the quadratic and sigma is
        the error.  This will be poorly matched to a linear fit for
        a3 != 0 """

    N = len(x)

    # randn gives samples from the "standard normal" distribution
    r = np.random.randn(N)

    y = a1 + a2*x + a3*x*x + sigma*r

    return y


def linear_regression(x, y, sigma, sigmaa=0):

    N = len(x)

    S = np.sum(1.0/sigma**2)

    xi_1 = np.sum(x/sigma**2)
    xi_2 = np.sum(x*x/sigma**2)

    eta = np.sum(y/sigma**2)
    mu = np.sum(x*y/sigma**2)

    a2 = (S*mu - xi_1*eta)/(xi_2*S - xi_1**2)
    a1 = (eta*xi_2 - mu*xi_1)/(xi_2*S - xi_1**2)

    chisq = np.sum( (a1 + a2*x - y)**2/sigma**2)
    chisq /= N-2

    if not sigmaa:
        return a1, a2, chisq

    else:
        sigmaFit = np.array([xi_2/(S*xi_2 - xi_1**2),
                                S/(S*xi_2 - xi_1**2)])
        sigmaFit = np.sqrt(sigmaFit)

        return a1, a2, chisq, sigmaFit


#-----------------------------------------------------------------------------
N = 40
x = np.linspace(0.0, 100.0, N)

#-----------------------------------------------------------------------------
# test 1 -- linear data

print("Linear fit to linear data\n")

# make up the experimental data with errors
sigma = 25.0*np.ones(N)

y = y_experiment(10.0, 3.0, sigma, x)

plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the linear regression
a1, a2, chisq, sigmaFit = linear_regression(x, y, sigma, sigmaa=1)

plt.plot(x, a1 + a2*x)

print("reduced chisq = ", chisq)
print(" a1 = %f +/- %f\n a2 = %f +/- %f\n" % (a1, sigmaFit[0], a2, sigmaFit[1]))

plt.savefig("linear-regression.png")


#-----------------------------------------------------------------------------
# test 2 -- quadratic data

plt.clf()

print("Linear fit to quadratic data\n")

# make up the experimental data with errors
sigma = 5.0*np.ones(N)

y = y_experiment2(2.0, 1.50, -0.02, sigma, x)

plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the linear regression
a1, a2, chisq = linear_regression(x, y, sigma)

plt.plot(x, a1 + a2*x)

print("reduced chisq = ", chisq)

plt.savefig("linear-regression-quad.png")
