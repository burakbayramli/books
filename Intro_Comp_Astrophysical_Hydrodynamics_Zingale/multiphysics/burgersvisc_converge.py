# test convergence of our inviscid Burgers problem by comparing against
# a high-resolution test case

import burgersvisc as bv
import matplotlib.pyplot as plt
import numpy as np

cfl = 0.8
tmax = 0.2
eps = 0.005

# high-resolution benchmark
nxb = 4096
gb = bv.Grid1d(nxb, ng=2, vars=["u"])

sb = bv.Simulation(gb)
sb.init_cond()
sb.evolve(eps, cfl, tmax, dovis=0)


# comparison simulations
N = [64, 128, 256, 512, 1024]

err = []
for nx in N:
    # our comparison -- a coarsen version of the high-resolution
    # solution
    gc = gb.restrict(fac=nxb//nx)

    g = bv.Grid1d(nx, ng=2, vars=["u"])
    s = bv.Simulation(g)
    s.init_cond()
    s.evolve(eps, cfl, tmax, dovis=0)

    uc = gc.data["u"]
    u = g.data["u"]

    e = uc - u
    err.append(g.norm(e))

N = np.array(N, dtype=np.float64)
err = np.array(err)

plt.scatter(N, err, color="C1", label="Burgers' solution")
plt.loglog(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="C0", 
           label="$\mathcal{O}(\Delta x^2)$")

plt.xlabel(r"$N$", fontsize="large")
plt.ylabel(r"L2 norm of absolute error")

plt.legend(frameon=False)

plt.tight_layout()
plt.savefig("burgersvisc_converge.pdf")
