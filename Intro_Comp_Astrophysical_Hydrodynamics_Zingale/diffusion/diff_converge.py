"""
solve the diffusion equation with explicit, backward-difference, and C-N
time discretization and compare the convergence rate for a Gaussian
profile
"""

import numpy as np
from scipy import linalg
import matplotlib.pyplot as plt

import diffusion_explicit as de
import diffusion_implicit as di
import diffusion_fo_implicit as dfi

if __name__ == "__main__":

    # Convergence of a Gaussian

    tmax = 0.005

    t0 = 1.e-4
    phi1 = 1.0
    phi2 = 2.0

    k = 1.0

    N = [16, 32, 64, 128, 256, 512]
    Cs = [0.8, 8.0]   # CFL numbers

    for C in Cs:

        err_i = []
        err_e = []
        err_fi = []

        for nx in N:

            gi = de.Grid1d(nx, ng=1)
            si = di.Simulation(gi, k=k)
            si.init_cond("gaussian", t0, phi1, phi2)
            si.evolve(C, tmax)

            ge = de.Grid1d(nx, ng=1)
            if C <= 1:
                se = de.Simulation(ge, k=k)
                se.init_cond("gaussian", t0, phi1, phi2)
                se.evolve(C, tmax)

            gfi = de.Grid1d(nx, ng=1)
            sfi = dfi.Simulation(gfi, k=k)
            sfi.init_cond("gaussian", t0, phi1, phi2)
            sfi.evolve(C, tmax)

            xc = 0.5*(ge.xmin + ge.xmax)
            phi_analytic = ge.phi_a(tmax, k, t0, phi1, phi2)

            err_i.append(gi.norm(gi.phi - phi_analytic))
            err_e.append(ge.norm(ge.phi - phi_analytic))
            err_fi.append(gfi.norm(gfi.phi - phi_analytic))

        plt.clf()

        plt.scatter(N, err_i, color="r", label="C-N implicit diffusion")
        plt.scatter(N, err_fi, color="b", label="backwards-difference diffusion")
        if C <= 1:
            plt.scatter(N, err_e, color="g", label="forward-difference (explicit) diffusion")

        N = np.array(N)
        plt.loglog(N, err_i[len(N)-1]*(N[len(N)-1]/N)**2, 
                   color="k", label="$\mathcal{O}(\Delta x^2)$")

        plt.xlabel(r"$N$", fontsize="large")
        plt.ylabel(r"L2 norm of absolute error")
        plt.title("diffusion convergence, C = {:3.2f}, t = {:5.2g}".format(C, tmax))

        plt.ylim(1.e-6, 1.e-1)
        plt.legend(frameon=False, loc="best", fontsize="small")

        plt.tight_layout()
        plt.savefig("diffimplicit-converge-{}.pdf".format(C))



