# plot the Hugoniot loci for a compressible Riemann problem

from __future__ import print_function

import matplotlib.pyplot as plt
import numpy as np
import riemann

import matplotlib as mpl

# Use LaTeX for rendering
mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'
mpl.rcParams['font.size'] = 12
mpl.rcParams['legend.fontsize'] = 'large'
mpl.rcParams['figure.titlesize'] = 'medium'

if __name__ == "__main__":

    # setup the problem -- Sod
    left = riemann.State(p=1.0, u=0.0, rho=1.0)
    right = riemann.State(p=0.1, u=0.0, rho=0.125)

    rp = riemann.RiemannProblem(left, right)
    rp.find_star_state()

    x, rho, u, p = rp.sample_solution(0.2, 128)

    plt.subplot(311)

    plt.plot(x, rho)

    plt.ylabel(r"$\rho$")

    plt.xlim(0, 1)
    plt.tick_params(axis="x", labelbottom="off")

    plt.subplot(312)

    plt.plot(x, u)

    plt.ylabel(r"$u$")

    plt.xlim(0, 1)
    plt.tick_params(axis="x", labelbottom="off")


    plt.subplot(313)

    plt.plot(x, p)

    plt.ylabel(r"$p$")
    plt.xlabel(r"$x$")

    plt.xlim(0, 1)

    f = plt.gcf()
    f.set_size_inches(6.0, 9.0)

    plt.tight_layout()

    plt.savefig("riemann-sod.pdf")



