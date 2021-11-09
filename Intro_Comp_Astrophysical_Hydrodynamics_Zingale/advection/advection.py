"""
2nd-order accurate finite-volume implementation of linear advection with
piecewise linear slope reconstruction.

We are solving a_t + u a_x = 0

This script defines two classes:

 -- the Grid1d class that manages a cell-centered grid and holds the
    data that lives on that grid

 -- the Simulation class that is built on a Grid1d object and defines
    everything needed to do a advection.

Options for several different slope limiters are provided.

M. Zingale

"""

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'


# helper functions for the limiting
def minmod(a, b):
    if abs(a) < abs(b) and a*b > 0.0:
        return a
    elif abs(b) < abs(a) and a*b > 0.0:
        return b
    else:
        return 0.0

def maxmod(a, b):
    if abs(a) > abs(b) and a*b > 0.0:
        return a
    elif abs(b) > abs(a) and a*b > 0.0:
        return b
    else:
        return 0.0


class Grid1d(object):

    def __init__(self, nx, ng, xmin=0.0, xmax=1.0):

        self.ng = ng
        self.nx = nx

        self.xmin = xmin
        self.xmax = xmax

        # python is zero-based.  Make easy intergers to know where the
        # real data lives
        self.ilo = ng
        self.ihi = ng+nx-1

        # physical coords -- cell-centered, left and right edges
        self.dx = (xmax - xmin)/(nx)
        self.x = xmin + (np.arange(nx+2*ng)-ng+0.5)*self.dx
        self.xl = xmin + (np.arange(nx+2*ng)-ng)*self.dx
        self.xr = xmin + (np.arange(nx+2*ng)+1.0)*self.dx

        # storage for the solution
        self.a = np.zeros((nx+2*ng), dtype=np.float64)


    def scratch_array(self):
        """ return a scratch array dimensioned for our grid """
        return np.zeros((self.nx+2*self.ng), dtype=np.float64)


    def fill_BCs(self):
        """ fill all single ghostcell with periodic boundary conditions """

        for n in range(self.ng):
            # left boundary
            self.a[self.ilo-1-n] = self.a[self.ihi-n]

            # right boundary
            self.a[self.ihi+1+n] = self.a[self.ilo+n]

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if len(e) != 2*self.ng + self.nx:
            return None

        #return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))
        return np.max(abs(e[self.ilo:self.ihi+1]))


class Simulation(object):

    def __init__(self, grid, u, C=0.8, slope_type="centered"):
        self.grid = grid
        self.t = 0.0 # simulation time
        self.u = u   # the constant advective velocity
        self.C = C   # CFL number
        self.slope_type = slope_type


    def init_cond(self, type="tophat"):
        """ initialize the data """
        if type == "tophat":
            self.grid.a[:] = 0.0
            self.grid.a[np.logical_and(self.grid.x >= 0.333,
                                       self.grid.x <= 0.666)] = 1.0

        elif type == "sine":
            self.grid.a[:] = np.sin(2.0*np.pi*self.grid.x/(self.grid.xmax-self.grid.xmin))

        elif type == "gaussian":
            al = 1.0 + np.exp(-60.0*(self.grid.xl - 0.5)**2)
            ar = 1.0 + np.exp(-60.0*(self.grid.xr - 0.5)**2)
            ac = 1.0 + np.exp(-60.0*(self.grid.x - 0.5)**2)
            
            self.grid.a[:] = (1./6.)*(al + 4*ac + ar)


    def timestep(self):
        """ return the advective timestep """
        return self.C*self.grid.dx/self.u


    def period(self):
        """ return the period for advection with velocity u """
        return (self.grid.xmax - self.grid.xmin)/self.u


    def states(self, dt):
        """ compute the left and right interface states """

        # compute the piecewise linear slopes
        g = self.grid
        slope = g.scratch_array()

        g = self.grid

        if self.slope_type == "godunov":

            # piecewise constant = 0 slopes
            slope[:] = 0.0

        elif self.slope_type == "centered":

            # unlimited centered difference slopes
            for i in range(g.ilo-1, g.ihi+2):
                slope[i] = 0.5*(g.a[i+1] - g.a[i-1])/g.dx

        elif self.slope_type == "minmod":

            # minmod limited slope
            for i in range(g.ilo-1, g.ihi+2):
                slope[i] = minmod( (g.a[i] - g.a[i-1])/g.dx,
                                   (g.a[i+1] - g.a[i])/g.dx )

        elif self.slope_type == "MC":

            # MC limiter
            for i in range(g.ilo-1, g.ihi+2):
                slope[i] = minmod(minmod( 2.0*(g.a[i] - g.a[i-1])/g.dx,
                                          2.0*(g.a[i+1] - g.a[i])/g.dx ),
                                  0.5*(g.a[i+1] - g.a[i-1])/g.dx)

        elif self.slope_type == "superbee":

            # superbee limiter
            for i in range(g.ilo-1, g.ihi+2):
                A = minmod( (g.a[i+1] - g.a[i])/g.dx,
                            2.0*(g.a[i] - g.a[i-1])/g.dx )

                B = minmod( (g.a[i] - g.a[i-1])/g.dx,
                            2.0*(g.a[i+1] - g.a[i])/g.dx )

                slope[i] = maxmod(A, B)


        # loop over all the interfaces.  Here, i refers to the left
        # interface of the zone.  Note that thre are 1 more interfaces
        # than zones
        al = g.scratch_array()
        ar = g.scratch_array()

        for i in range(g.ilo, g.ihi+2):

            # left state on the current interface comes from zone i-1
            al[i] = g.a[i-1] + 0.5*g.dx*(1.0 - u*dt/g.dx)*slope[i-1]

            # right state on the current interface comes from zone i
            ar[i] = g.a[i] - 0.5*g.dx*(1.0 + u*dt/g.dx)*slope[i]

        return al, ar


    def riemann(self, al, ar):
        """
        Riemann problem for advection -- this is simply upwinding,
        but we return the flux
        """

        if self.u > 0.0:
            return self.u*al
        else:
            return self.u*ar


    def update(self, dt, flux):
        """ conservative update """

        g = self.grid

        anew = g.scratch_array()

        anew[g.ilo:g.ihi+1] = g.a[g.ilo:g.ihi+1] + \
            dt/g.dx * (flux[g.ilo:g.ihi+1] - flux[g.ilo+1:g.ihi+2])

        return anew


    def evolve(self, num_periods=1):
        """ evolve the linear advection equation """
        self.t = 0.0
        g = self.grid

        tmax = num_periods*self.period()


        # main evolution loop
        while self.t < tmax:

            # fill the boundary conditions
            g.fill_BCs()

            # get the timestep
            dt = self.timestep()

            if self.t + dt > tmax:
                dt = tmax - self.t

            # get the interface states
            al, ar = self.states(dt)

            # solve the Riemann problem at all interfaces
            flux = self.riemann(al, ar)

            # do the conservative update
            anew = self.update(dt, flux)

            g.a[:] = anew[:]

            self.t += dt


if __name__ == "__main__":


    #-------------------------------------------------------------------------
    # compare limiting and no-limiting

    xmin = 0.0
    xmax = 1.0
    nx = 64
    ng = 2

    g = Grid1d(nx, ng, xmin=xmin, xmax=xmax)

    u = 1.0

    s = Simulation(g, u, C=0.7, slope_type="centered")
    s.init_cond("tophat")
    ainit = s.grid.a.copy()
    s.evolve(num_periods=5)

    plt.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1],
             ls=":", label="exact")

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1],
             label="unlimited")

    s = Simulation(g, u, C=0.7, slope_type="minmod")
    s.init_cond("tophat")
    s.evolve(num_periods=5)

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1],
             label="minmod limiter")


    plt.legend(frameon=False, loc="best")

    plt.xlabel(r"$x$")
    plt.ylabel(r"$a$")

    plt.savefig("fv-advect.pdf")



    #-------------------------------------------------------------------------
    # convergence test
    problem = "gaussian"

    xmin = 0.0
    xmax = 1.0
    ng = 2
    N = [32, 64, 128, 256, 512]

    err_god = []
    err_nolim = []
    err_lim = []
    err_lim2 = []

    u = 1.0

    for nx in N:

        # no limiting
        gg = Grid1d(nx, ng, xmin=xmin, xmax=xmax)
        sg = Simulation(gg, u, C=0.8, slope_type="godunov")
        sg.init_cond("gaussian")
        ainit = sg.grid.a.copy()
        sg.evolve(num_periods=5)

        err_god.append(gg.norm(gg.a - ainit))

        # no limiting
        gu = Grid1d(nx, ng, xmin=xmin, xmax=xmax)
        su = Simulation(gu, u, C=0.8, slope_type="centered")
        su.init_cond("gaussian")
        ainit = su.grid.a.copy()
        su.evolve(num_periods=5)

        err_nolim.append(gu.norm(gu.a - ainit))

        # MC limiting
        gl = Grid1d(nx, ng, xmin=xmin, xmax=xmax)
        sl = Simulation(gl, u, C=0.8, slope_type="MC")
        sl.init_cond("gaussian")
        ainit = sl.grid.a.copy()
        sl.evolve(num_periods=5)

        err_lim.append(gl.norm(gl.a - ainit))

        # minmod limiting
        gl2 = Grid1d(nx, ng, xmin=xmin, xmax=xmax)
        sl2 = Simulation(gl2, u, C=0.8, slope_type="minmod")
        sl2.init_cond("gaussian")
        ainit = sl2.grid.a.copy()
        sl2.evolve(num_periods=5)

        err_lim2.append(gl2.norm(gl2.a - ainit))

        print(g.dx, nx, err_nolim[-1], err_lim[-1], err_lim2[-1])


    plt.clf()

    N = np.array(N, dtype=np.float64)
    err_nolim = np.array(err_nolim)
    err_lim = np.array(err_lim)
    err_lim2 = np.array(err_lim2)

    plt.scatter(N, err_god, label="Godunov", color="C0")
    plt.scatter(N, err_nolim, label="unlimited center", color="C1")
    plt.scatter(N, err_lim, label="MC", color="C2")
    plt.scatter(N, err_lim2, label="minmod", color="C3")
    plt.plot(N, err_god[len(N)-1]*(N[len(N)-1]/N),
             color="k", label=r"$\mathcal{O}(\Delta x)$")
    plt.plot(N, err_nolim[len(N)-1]*(N[len(N)-1]/N)**2,
             color="0.5", label=r"$\mathcal{O}(\Delta x^2)$")

    ax = plt.gca()
    ax.set_xscale('log')
    ax.set_yscale('log')

    plt.xlabel("N")
    plt.ylabel(r"$\| a^\mathrm{final} - a^\mathrm{init} \|_2$",
               fontsize=16)

    plt.legend(frameon=False, loc="best", fontsize="small")

    plt.savefig("plm-converge.pdf")


    #-------------------------------------------------------------------------
    # different limiters: run both the Gaussian and tophat

    xmin = 0.0
    xmax = 1.0
    nx = 32
    ng = 2

    u = 1.0

    g= Grid1d(nx, ng, xmin=xmin, xmax=xmax)

    for p in ["gaussian", "tophat"]:
        plt.clf()

        s = Simulation(g, u, C=0.8, slope_type="godunov")
        s.init_cond(p)
        ainit = s.grid.a.copy()

        s.evolve(num_periods=5)

        plt.subplot(231)

        plt.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1], ls=":")
        plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

        plt.title("piecewise constant")


        s = Simulation(g, u, C=0.8, slope_type="centered")
        s.init_cond(p)
        ainit = s.grid.a.copy()

        s.evolve(num_periods=5)

        plt.subplot(232)

        plt.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1], ls=":")
        plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

        plt.title("centered (unlimited)")


        s = Simulation(g, u, C=0.8, slope_type="minmod")
        s.init_cond(p)
        ainit = s.grid.a.copy()

        s.evolve(num_periods=5)

        plt.subplot(233)

        plt.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1], ls=":")
        plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

        plt.title("minmod limiter")


        s = Simulation(g, u, C=0.8, slope_type="MC")
        s.init_cond(p)
        ainit = s.grid.a.copy()

        s.evolve(num_periods=5)

        plt.subplot(234)

        plt.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1], ls=":")
        plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

        plt.title("MC limiter")


        s = Simulation(g, u, C=0.8, slope_type="superbee")
        s.init_cond(p)
        ainit = s.grid.a.copy()

        s.evolve(num_periods=5)

        plt.subplot(235)

        plt.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1], ls=":")
        plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

        plt.title("superbee limiter")


        f = plt.gcf()
        f.set_size_inches(10.0,7.0)

        plt.tight_layout()

        plt.savefig("fv-{}-limiters.pdf".format(p), bbox_inches="tight")
