# finite-difference implementation of first-order upwind method for
# linear advection
#
# We are solving a_t + u a_x = 0
#
# M. Zingale (2013-03-21)

import matplotlib as mpl
mpl.rcParams["mathtext.fontset"] = "cm"
mpl.rcParams["mathtext.rm"] = "serif"

import fdadvect as adv
import numpy as np
import matplotlib.pyplot as plt




runs = [("tophat", 5), ("tophat", 1), ("sine", 5), ("sine", 1)]

for ic, num_periods in runs:

    plt.clf()

    # create the grid
    nx = 65
    ng = 1
    g = adv.FDGrid(nx, ng)


    # define the CFL and speed
    C = 0.9
    u = 1.0

    # evolve
    adv.solve_advection(g, u, C, num_periods=num_periods,
                        method="upwind", init_cond=ic)

    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":")
    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

    plt.xlabel(r"$x$")
    plt.ylabel(r"$a$")

    plt.savefig("fd-upwind-{}-{}T.png".format(ic, num_periods))

