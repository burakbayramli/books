# plot the Hugoniot loci for a compressible Riemann problem

from __future__ import print_function

import numpy as np
import riemann


def store(left, right, time, gamma, outfile="riemann.out"):

    rp = riemann.RiemannProblem(left, right, gamma=gamma)
    rp.find_star_state()

    x, rho, u, p = rp.sample_solution(time, 128)

    e = p/rho/(gamma - 1.0)

    # output the solution
    with open(outfile, "w") as f:
        f.write("# left: {}\n".format(left))
        f.write("# right: {}\n".format(right))
        f.write("# gamma = {}\n".format(gamma))
        f.write("# time = {}\n".format(time))
        f.write("# {:^20} {:^20} {:^20} {:^20} {:^20}\n".format("x", "rho", "u", "p", "e"))
        for n in range(len(x)):
            f.write("  {:20.10g} {:20.10g} {:20.10g} {:20.10g} {:20.10g}\n".format(x[n], rho[n], u[n], p[n], e[n]))


# Sod
left = riemann.State(p=1.0, u=0.0, rho=1.0)
right = riemann.State(p=0.1, u=0.0, rho=0.125)
time = 0.2
gamma = 1.4
outfile = "sod_exact.out"

store(left, right, time, gamma, outfile=outfile)

# double rarefaction
left = riemann.State(p=0.4, u=-2.0, rho=1.0)
right = riemann.State(p=0.4, u=2.0, rho=1.0)
time = 0.15
gamma = 1.4
outfile = "double_rarefaction_exact.out"

store(left, right, time, gamma, outfile=outfile)

# strong shock
left = riemann.State(p=1000.0, u=0.0, rho=1.0)
right = riemann.State(p=0.01, u=0.0, rho=1.0)
time = 0.012
gamma = 1.4
outfile = "strong_shock_exact.out"

store(left, right, time, gamma, outfile=outfile)

# slow shock
left = riemann.State(p=100.0, u=-1.4701, rho=5.6698)
right = riemann.State(p=1.0, u=-10.5, rho=1.0)
time = 1.0
gamma = 1.4
outfile = "slow_shock_exact.out"

store(left, right, time, gamma, outfile=outfile)
