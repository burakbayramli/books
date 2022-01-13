# use simulated annealing to find the minimum of a function

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import random
import scipy.optimize as optimize

def f1(x):
    return x**2 - np.cos(4.0*np.pi*x)


def f2(x):
    return np.cos(x) + np.cos(np.sqrt(2.0)*x) + np.cos(np.sqrt(3.0)*x)


def sa_minimize(fun, xmin=0.0, xmax=50.0):

    Tmin = 1.e-3
    Tmax = 1000.0
    tau = 1.e4

    t = 0
    T = Tmax

    # initial guess
    x_old = np.random.uniform(xmin, xmax)
    f_old = fun(x_old)

    # cooling loop
    while T > Tmin:
        T = Tmax*np.exp(-t/tau)

        # perturb x
        x_new = min(xmax, max(xmin, x_old + np.random.normal()))
        f_new = fun(x_new)

        # check whether we should keep it
        if random.random() < np.exp(-(f_new - f_old)/T):
            x_old = x_new
            f_old = f_new

        t += 1

    return x_old, f_old


def do_comparison(fun, xmin, xmax, outfile="min.png"):

    plt.clf()

    # our method
    x0, f0 = sa_minimize(fun, xmin=xmin, xmax=xmax)

    # SciPy's method -- this returns an object with the info
    fx0 = optimize.minimize_scalar(fun)

    # plot the function
    xx = np.linspace(xmin, xmax, 1000)
    plt.plot(xx, fun(xx))

    plt.scatter(x0, f0, label="our MCMC method")
    plt.scatter(fx0.x, fun(fx0.x), label="scipy optimize", marker="x")

    plt.legend(frameon=False)

    print("our method: ({}, {}); scipy: ({}, {})".format(x0, f0, fx0.x, fun(fx0.x)))

    plt.savefig(outfile, dpi=150)


if __name__ == "__main__":

    do_comparison(f1, -5.0, 5.0, outfile="min_f1.png")
    do_comparison(f2, 0.0, 50.0, outfile="min_f2.png")


