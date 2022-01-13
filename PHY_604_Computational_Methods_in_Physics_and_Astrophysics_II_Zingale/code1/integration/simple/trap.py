# trapezoidal rule
#
# M. Zingale (2013-02-13)

from __future__ import print_function

import numpy as np

def fun(x):
    """ function we wish to integrate """
    return np.exp(-x)

def I_exact(a, b):
    """ analytic value of the integral """
    return -np.exp(-b) + np.exp(-a)

def trap(a, b, f, N):
    """ do a trapezoid integration by breaking up the domain [a,b] into N
    slabs """
    xedge = np.linspace(a, b, N+1)

    integral = 0.0

    for n in range(N):
        integral += 0.5*(xedge[n+1] - xedge[n])*(f(xedge[n]) + f(xedge[n+1]))

    return integral


def main():
    """ loop over a number of resolutions and output the error """
    a = 0.0
    b = 1.0

    for N in [2, 4, 8, 16, 32, 64, 128]:
        t = trap(a, b, fun, N)
        e = t - I_exact(a, b)
        print(N, t, e)


if __name__ == "__main__":
    main()
