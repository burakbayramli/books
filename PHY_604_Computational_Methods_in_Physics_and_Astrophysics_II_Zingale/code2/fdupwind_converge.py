# finite-difference implementation of first-order upwind method for
# linear advection
#
# We are solving a_t + u a_x = 0
#
# M. Zingale (2013-03-21)

import matplotlib.pyplot as plt
import numpy as np
import fdupwind

N = [17, 33, 65, 129, 257]
err = []

for nx in N:

    #create the grid
    ng = 1
    g = fdupwind.FDGrid(nx, ng)

    # define the CFL and speed
    C = 0.8
    u = 1.0

    # time info
    tmax = 1.0*(g.xmax - g.xmin)/u

    # initialize
    g.init_cond("sine")

    # evolve
    fdupwind.evolve_upwind(g, C, u, tmax)

    err.append(g.norm(g.a - g.ainit))
    print err[-1]


N = np.array(N, dtype=np.float64)
err = np.array(err)

plt.scatter(N, err, color="r")
plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**1, color="k")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel(r"$N$")
plt.ylabel(r"$|| a_i - a^\mathrm{init}_i ||_2$")

plt.savefig("upwind-converge.png")
