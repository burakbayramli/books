# finite-difference implementation of the diffusion equation with first-order
# explicit time discretization
#
# We are solving phi_t = k phi_xx
#
# We run at several resolutions and compute the error.  This uses a
# cell-centered finite-difference grid
#
# M. Zingale (2013-04-07)

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

import matplotlib as mpl

# Use LaTeX for rendering
mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'
mpl.rcParams['font.size'] = 12
mpl.rcParams['legend.fontsize'] = 'medium'
mpl.rcParams['figure.titlesize'] = 'small'

class Grid1d(object):

    def __init__(self, nx, ng=1, xmin=0.0, xmax=1.0):
        """ grid class initialization """

        self.nx = nx
        self.ng = ng

        self.xmin = xmin
        self.xmax = xmax

        self.ilo = ng
        self.ihi = ng+nx-1

        self.dx = (xmax - xmin)/nx
        self.x = xmin + (np.arange(nx+2*ng) -ng + 0.5)*self.dx

        # storage for the solution
        self.phi = np.zeros((nx+2*ng), dtype=np.float64)

    def scratch_array(self):
        return np.zeros((2*self.ng+self.nx), dtype=np.float64)

    def fill_BCs(self):
        """ fill the Neumann BCs """
        self.phi[0:self.ilo]  = self.phi[self.ilo]
        self.phi[self.ihi+1:] = self.phi[self.ihi]

    def phi_a(self, t, k, t0, phi1, phi2):
        """ analytic solution for the diffusion of a Gaussian """
        xc = 0.5*(self.xmin + self.xmax)
        return (phi2 - phi1)*np.sqrt(t0/(t + t0)) * \
            np.exp(-0.25*(self.x-xc)**2/(k*(t + t0))) + phi1

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))


class Simulation(object):

    def __init__(self, grid, k=1.0):
        self.grid = grid
        self.t = 0.0
        self.k = k  # diffusion coefficient

    def init_cond(self, name, *args):
        # initialize the data

        if name == "gaussian":
            t0, phi1, phi2 = args
            self.grid.phi[:] = self.grid.phi_a(0.0, self.k, t0, phi1, phi2)

    def evolve(self, C, tmax):

        gr = self.grid

        # time info
        dt = C*0.5*gr.dx**2/self.k

        phinew = gr.scratch_array()

        while self.t < tmax:

            # make sure we end right at tmax
            if self.t + dt > tmax:
                dt = tmax - self.t

            # fill the boundary conditions
            gr.fill_BCs()

            alpha = self.k*dt/gr.dx**2

            # loop over zones
            for i in range(gr.ilo, gr.ihi+1):

                # explicit diffusion
                phinew[i] = gr.phi[i] + \
                            alpha*(gr.phi[i+1] - 2.0*gr.phi[i] + gr.phi[i-1])

            # store the updated solution
            gr.phi[:] = phinew[:]
            self.t += dt


if __name__ == "__main__":

    #-----------------------------------------------------------------------------
    # diffusion coefficient
    k = 1.0

    # reference time
    t0 = 1.e-4

    # state coeffs
    phi1 = 1.0
    phi2 = 2.0

    # solution at multiple times

    # a characteristic timescale for diffusion if L^2/k
    tmax = 0.0008

    nx = 64

    C = 0.8

    ntimes = 4
    tend = tmax/10.0**ntimes

    c = ["C0", "C1", "C2", "C3", "C4"]

    while tend <= tmax:

        g = Grid1d(nx, ng=2)
        s = Simulation(g, k=k)
        s.init_cond("gaussian", t0, phi1, phi2)
        s.evolve(C, tend)

        phi_analytic = g.phi_a(tend, k, t0, phi1, phi2)

        color = c.pop()
        plt.plot(g.x[g.ilo:g.ihi+1], g.phi[g.ilo:g.ihi+1],
                 "x", color=color, label="$t = %g$ s" % (tend))
        plt.plot(g.x[g.ilo:g.ihi+1], phi_analytic[g.ilo:g.ihi+1],
                 color=color, ls=":")

        tend = 10.0*tend

    plt.xlim(0.35,0.65)

    plt.legend(frameon=False, fontsize="medium")

    plt.xlabel("$x$", fontsize="large")
    plt.ylabel(r"$\phi$", fontsize="large")
    plt.title("explicit diffusion, nx = {}, C = {:3.2f}".format(nx, C), fontsize="small")

    plt.savefig("diff-explicit-{}.pdf".format(nx))

    #-----------------------------------------------------------------------------
    # convergence

    plt.clf()

    # a characteristic timescale for diffusion is L^2/k
    tmax = 0.005

    t0 = 1.e-4
    phi1 = 1.0
    phi2 = 2.0

    k = 1.0

    N = [16, 32, 64, 128, 256, 512]

    # CFL number
    C = 0.8

    err = []

    for nx in N:

        # the present C-N discretization
        g = Grid1d(nx, ng=1)
        s = Simulation(g, k=k)
        s.init_cond("gaussian", t0, phi1, phi2)
        s.evolve(C, tmax)

        phi_analytic = g.phi_a(tmax, k, t0, phi1, phi2)

        err.append(g.norm(g.phi - phi_analytic))

        plt.plot(g.x[g.ilo:g.ihi+1], g.phi[g.ilo:g.ihi+1], label="N = %d" % (nx))

    plt.legend(frameon=False)
    plt.xlabel("$x$", fontsize="large")
    plt.ylabel(r"$\phi$", fontsize="large")
    plt.title("Explicit diffusion, C = {:3.2f}, t = {:5.2g}".format(C, tmax), fontsize="small")

    plt.savefig("diffexplicit-res.pdf")

    plt.clf()

    N = np.array(N, dtype=np.float64)
    err = np.array(err)

    plt.scatter(N, err, color="C1", label="explicit diffusion")
    plt.loglog(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="C0", label="$\mathcal{O}(\Delta x^2)$")

    plt.xlabel(r"$N$", fontsize="large")
    plt.ylabel(r"L2 norm of absolute error")
    plt.title("Convergence of Explicit Diffusion, C = %3.2f, t = %5.2g" % (C, tmax), fontsize="small")

    plt.ylim(1.e-6, 1.e-1)
    plt.legend(frameon=False, fontsize="small")

    plt.savefig("diffexplicit-converge-{}.pdf".format(C))


    #-----------------------------------------------------------------------------
    # exceed the timestep limit

    plt.clf()

    # a characteristic timescale for diffusion is L^2/k
    tmax = 0.005

    nx = 64

    C = 2.0

    g = Grid1d(nx, ng=2)
    s = Simulation(g, k=k)
    s.init_cond("gaussian", t0, phi1, phi2)
    s.evolve(C, tend)

    phi_analytic = g.phi_a(tend, k, t0, phi1, phi2)

    plt.plot(g.x[g.ilo:g.ihi+1], g.phi[g.ilo:g.ihi+1],
             "x-", color="C0", label="$t = %g$ s" % (tend))
    plt.plot(g.x[g.ilo:g.ihi+1], phi_analytic[g.ilo:g.ihi+1],
             color="0.5", ls=":")

    plt.xlim(0.35,0.65)
    plt.xlabel("$x$", fontsize="large")
    plt.ylabel(r"$\phi$", fontsize="large")
    plt.title("explicit diffusion, nx = %d, C = %3.2f, t = %5.2g" % (nx, C, tmax), fontsize="small")

    ax = plt.gca()
    ax.xaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))
    ax.yaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))

    plt.savefig("diff-explicit-64-bad.pdf")
