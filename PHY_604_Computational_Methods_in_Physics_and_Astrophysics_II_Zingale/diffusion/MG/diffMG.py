#!/usr/bin/env python

"""

Solve the diffusion equation with Crank-Nicolson time-discretization
using MG.

M. Zingale

"""
from __future__ import print_function

import numpy as np
import patch1d
import multigrid
import matplotlib.pyplot as plt


# global parameters for the analytic solution
phi1 = 1.0
phi2 = 2.0

# t0 is the initial time in the initial conditions -- the smaller you
# make this, the more the initial conditions approximate a delta
# function.  Careful though, too small, and you will not resolve this
# on your grid.
t0 = 1.e-4

# diffusion coefficient
k = 1.0

# the analytic solution
def phi_a(myg, t):
    xc = 0.5*(myg.xmin + myg.xmax)

    return (phi2 - phi1)*np.sqrt(t0/(t + t0)) * \
            np.exp(-0.25*(myg.x-xc)**2/(k*(t + t0))) + phi1


# the L2 error norm
def error(myg, r):

    # L2 norm of elements in r, multiplied by dx to
    # normalize
    return np.sqrt(myg.dx*np.sum((r[myg.ilo:myg.ihi+1]**2)))



def lap(gr, phi):
    """ compute the Laplacian of phi """

    lapphi = gr.scratch_array()

    ib = gr.ilo
    ie = gr.ihi

    lapphi[ib:ie+1] = (phi[ib-1:ie] - 2.0*phi[ib:ie+1] + phi[ib+1:ie+2])/gr.dx**2

    return lapphi



def evolve(nx, C, tmax, xmax=None):

    xmin = 0.0
    if xmax == None: xmax = 1.0

    # create a dummy patch to store some info in the same way the MG
    # solver will
    myg = patch1d.Grid1d(nx, ng=1, xmin=xmin, xmax=xmax)

    # initialize the data
    phi = myg.scratch_array()

    # initial solution -- this fills the GC too
    phi[:] = phi_a(myg, 0.0)

    # time info
    dt = C*0.5*myg.dx**2/k
    t = 0.0

    # evolve
    while t < tmax:

        if t + dt > tmax:
            dt = tmax - t

        # create the multigrid object
        a = multigrid.CellCenterMG1d(nx, xmin=xmin, xmax=xmax,
                                     alpha = 1.0, beta = 0.5*dt*k,
                                     xl_BC_type="neumann", xr_BC_type="neumann",
                                     verbose=0)

        # initialize the RHS
        a.init_RHS(phi + 0.5*dt*k*lap(a.soln_grid, phi))

        # initialize the solution to 0
        a.init_zeros()

        # solve to a relative tolerance of 1.e-11
        a.solve(rtol=1.e-11)

        # get the solution
        v = a.get_solution()

        # store the new solution
        phi[:] = v[:]

        t += dt

    return a.soln_grid, phi



def compare_CFLs(nx, tmax, ofile="diff_mg.png"):
    """ run diffusion with various CFS """

    xmax = 1.0

    plt.clf()

    Cs = [0.8, 2.0, 10.0]
    for C in Cs:
        gr, phi = evolve(nx, C, tmax, xmax=xmax)
        plt.plot(gr.x[gr.ilo:gr.ihi+1], phi[gr.ilo:gr.ihi+1], label = "C = {}".format(C))

    plt.plot(gr.x[gr.ilo:gr.ihi+1],
             phi_a(gr, tmax)[gr.ilo:gr.ihi+1],
             ls=":", color="0.5", label="analytic solution", lw=2)

    plt.legend(frameon=False)

    plt.xlim(0.0, 1.0)

    plt.xlabel("$x$")
    plt.ylabel(r"$\phi$")
    plt.title("C-N implicit diffusion via MG, nx = {}, t = {:5.2g}".format(nx, tmax))

    plt.savefig(ofile, dpi=150)


def convergence():
    """ run a convergence test """

    plt.clf()

    # a characteristic timescale for diffusion if L^2/k
    tmax = 5.e-3

    N = [16, 32, 64, 128, 256, 512]

    # CFL number
    C = 0.8

    err = []

    for nx in N:
        g, phi = evolve(nx, C, tmax)
        phi_analytic = phi_a(g, tmax)
        err.append(error(g, phi - phi_analytic))

    N = np.array(N, dtype=np.float64)
    err = np.array(err)

    plt.scatter(N, err, label="C-N implicit diffusion")
    plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, ls=":", 
             label=r"$\mathcal{O}(\Delta x^2)$", color="C1")

    ax = plt.gca()
    ax.set_xscale('log')
    ax.set_yscale('log')

    plt.xlabel(r"$N$")
    plt.ylabel(r"L2 norm of absolute error")
    plt.title("Convergence of C-N MG Diffusion, C = {}".format(C))

    plt.legend(frameon=False)

    plt.savefig("diff_mg_converge.png", dpi=150)



compare_CFLs(128, 5.e-3)
compare_CFLs(128, 5.e-4, ofile="diff_mg_early.png")
convergence()
