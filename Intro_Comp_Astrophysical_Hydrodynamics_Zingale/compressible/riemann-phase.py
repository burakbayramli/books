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
    rp.plot_hugoniot()

    plt.savefig("riemann-phase.pdf")

    print(rp.pstar, rp.ustar)
