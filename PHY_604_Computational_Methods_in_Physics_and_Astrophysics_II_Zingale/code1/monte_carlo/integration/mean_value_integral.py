# This example comes from Newman section 10.2

# calculate the I = int_0^2 sin^2[ 1/(x (2-x)) ] dx
#
# We do this via mean-value integration

from __future__ import print_function

import numpy as np
import scipy.integrate as integrate
import random


def f(x):
    return np.sin(1.0/(x*(2.0 - x)))**2



if __name__ == "__main__":

    xmin = 0.0
    xmax = 2.0

    eps = 1.e-16

    # Gaussian quadrature
    I, err = integrate.quadrature(f, xmin, xmax)
    print(I, err)

    # quadpack
    I, err = integrate.quad(f, xmin+eps, xmax-eps)
    print(I, err)


    # Monte Carlo
    for N in [1000, 10000, 100000, 1000000]:

        f_avg = 0.0
        for n in range(N):
            x = xmax*random.random()
            f_avg += f(x)

        I = (xmax - xmin)*f_avg/N

        print(N, I)
