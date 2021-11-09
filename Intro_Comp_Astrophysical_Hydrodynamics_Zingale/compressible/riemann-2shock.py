# plot the two shock solution in the Hugoniot plane for a compressible
# Riemann problem

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
    rp.find_2shock_star_state()
    rp.plot_2shock_hugoniot()

    print("2 shock: ", rp.pstar, rp.ustar)

    rp.find_star_state()
    rp.plot_hugoniot(gray=True)

    print("normal: ", rp.pstar, rp.ustar)

    plt.savefig("riemann-2shock-sod-phase.pdf")

    plt.clf()

    # setup the problem -- double rarefaction
    left = riemann.State(p=1.0, u=-1.0, rho=1.0)
    right = riemann.State(p=1.0, u=1.0, rho=1.0)

    rp = riemann.RiemannProblem(left, right)
    rp.find_2shock_star_state()
    rp.plot_2shock_hugoniot()

    rp.find_star_state()
    rp.plot_hugoniot(gray=True)

    plt.savefig("riemann-2shock-doublerare-phase.pdf")

    print(rp.pstar, rp.ustar)
