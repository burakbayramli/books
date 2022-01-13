# This example comes from Newman section 10.2

# calculate the I = int_0^2 sin^2[ 1/(x (2-x)) ] dx
#
# This is bounded by 1

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import scipy.integrate as integrate
import random

def f(x):
    return np.sin(1.0/(x*(2.0 - x)))**2



if __name__ == "__main__":

    # make a plot first
    eps = 1.e-15
    x = np.linspace(eps, 2.0-eps, 10000)
    fn = f(x)

    plt.fill_between(x, fn)
    plt.xlabel(r"$x$")
    plt.ylabel(r"$f(x)$")

    plt.savefig("integrand.png")

    xmin = 0.0
    xmax = 2.0

    # Gaussian quadrature
    I, err = integrate.quadrature(f, xmin, xmax)
    print(I, err)

    # quadpack
    I, err = integrate.quad(f, xmin+eps, xmax-eps)
    print(I, err)

    # Monte Carlo
    for N in [1000, 10000, 100000, 1000000]:
        A = 2.0   # the area of the domain we are sampling

        count = 0
        for n in range(N):
            x = xmax*random.random()
            y = random.random()

            if y < f(x):
                count += 1

        I = A*float(count)/N

        print(N, I)
