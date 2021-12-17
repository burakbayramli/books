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




N = [17, 33, 65, 129, 257]
err = []

for nx in N:

    # create the grid
    ng = 1
    g = adv.FDGrid(nx, ng)


    # define the CFL and speed
    C = 0.8
    u = 1.0

    # evolve
    adv.solve_advection(g, u, C, num_periods=1,
                        method="upwind", init_cond="sine")

    err.append(g.norm(g.a - g.ainit))

N = np.array(N)
plt.scatter(N, err)
plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**1)

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel(r"$N$")
plt.ylabel(r"$|| a_i - a^\mathrm{init}_i ||_2$")

plt.tight_layout()
plt.savefig("upwind-converge.png", dpi=150)


