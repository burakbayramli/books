# Simpson's rule
#
# M. Zingale (2013-02-13)

from __future__ import print_function

import math
import numpy as np
import sys

def fun(x):
    """ function we wish to integrate """
    return np.exp(-x)

def I_exact(a,b):
    """ analytic value of the integral """
    return -math.exp(-b) + math.exp(-a)

def simp(a, b, f, N):
    """  do a Simpson's integration by breaking up the domain [a,b] into N
    slabs.  Note: N must be even, because we do a pair at a time """

    xedge = np.linspace(a, b, N+1)

    integral = 0.0

    if not N%2 == 0:
        sys.exit("ERROR: N must be even")

    delta = (xedge[1] - xedge[0])

    for n in range(0, N, 2):
        integral += (1.0/3.0)*delta*(f(xedge[n]) +
                                     4.0*f(xedge[n+1]) +
                                     f(xedge[n+2]))

    return integral



def main():
    """ loop over a number of resolutions and output the error """
    a = 0.0
    b = 1.0

    for N in [2, 4, 8, 16, 32, 64, 128]:
        t = simp(a, b, fun, N)
        e = t - I_exact(a, b)
        print(N, t, e)


if __name__ == "__main__":
    main()
