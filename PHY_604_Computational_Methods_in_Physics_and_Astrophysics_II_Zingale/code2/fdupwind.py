# finite-difference implementation of first-order upwind method for
# linear advection
#
# We are solving a_t + u a_x = 0
#
# M. Zingale (2013-03-21)

import numpy as np
import matplotlib.pyplot as plt

class FDGrid(object):

    def __init__(self, nx, ng, xmin=0.0, xmax=1.0):

        self.xmin = xmin
        self.xmax = xmax
        self.ng = ng
        self.nx = nx

        # python is zero-based.  Make easy intergers to know where the
        # real data lives
        self.ilo = ng
        self.ihi = ng+nx-1

        # physical coords
        self.dx = (xmax - xmin)/(nx-1)
        self.x = xmin + (np.arange(nx+2*ng)-ng)*self.dx

        # storage for the solution
        self.a = np.zeros((nx+2*ng), dtype=np.float64)

    def scratch_array(self):
        """ return a scratch array dimensioned for our grid """
        return np.zeros((self.nx+2*self.ng), dtype=np.float64)

    def fill_BCs(self):
        """ fill the a single ghostcell with periodic boundary conditions """
        self.a[self.ilo-1] = self.a[self.ihi-1]
        self.a[self.ihi+1] = self.a[self.ilo+1]

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))

    def init_cond(self, type="tophat"):

        if type == "tophat":
            self.a[np.logical_and(self.x >= 0.333, self.x <= 0.666)] = 1.0
        elif type == "sine":
            self.a[:] = np.sin(2.0*np.pi*self.x/(self.xmax-self.xmin))
        else:
            print "unknown type"

        self.ainit = self.a.copy()


def evolve_upwind(g, C, u, tmax):

    t = 0.0
    dt = C*g.dx/u

    # evolution loop
    anew = g.scratch_array()

    g.a[:] = g.ainit[:]

    while t < tmax:

        if t + dt > tmax:
            dt = tmax - t
            C = u*dt/g.dx

        # fill the boundary conditions
        g.fill_BCs()

        # loop over zones: note since we are periodic and both endpoints
        # are on the computational domain boundary, we don't have to
        # update both g.ilo and g.ihi -- we could set them equal instead.
        # But this is more general
        for i in range(g.ilo, g.ihi+1):

            # upwind
            anew[i] = g.a[i] - C*(g.a[i] - g.a[i-1])

        # store the updated solution
        g.a[:] = anew[:]

        t += dt


if __name__ == "__main__":

    #-----------------------------------------------------------------------------
    # tophat, 1 period:

    #create the grid
    nx = 65
    ng = 1
    g = FDGrid(nx, ng)


    # define the CFL and speed
    C = 0.8
    u = 1.0

    # time info
    tmax = 1.0*(g.xmax - g.xmin)/u

    # initialize
    g.init_cond("tophat")

    # evolve
    evolve_upwind(g, C, u, tmax)


    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":")
    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

    plt.xlabel(r"$x$")
    plt.ylabel(r"$a$")

    plt.savefig("fd-upwind-tophat.png")


    #-----------------------------------------------------------------------------
    # tophat, 5 periods:

    plt.clf()

    # create the grid
    nx = 65
    ng = 1
    g = FDGrid(nx, ng)


    # define the CFL and speed
    C = 0.9
    u = 1.0

    # time info
    tmax = 5.0*(g.xmax - g.xmin)/u

    # initialize
    g.init_cond("tophat")

    # evolve
    evolve_upwind(g, C, u, tmax)


    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":")
    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

    plt.xlabel(r"$x$")
    plt.ylabel(r"$a$")

    plt.savefig("fd-upwind-tophat-5T.png")


    #-----------------------------------------------------------------------------
    # sine, 1 period:

    plt.clf()

    # create the grid
    nx = 65
    ng = 1
    g = FDGrid(nx, ng)


    # define the CFL and speed
    C = 0.9
    u = 1.0

    # time info
    tmax = 1.0*(g.xmax - g.xmin)/u

    # initialize
    g.init_cond("sine")

    # evolve
    evolve_upwind(g, C, u, tmax)


    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":")
    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

    plt.xlabel(r"$x$")
    plt.ylabel(r"$a$")

    plt.savefig("fd-upwind-sine.png")


    #-----------------------------------------------------------------------------
    # sine, 5 periods:

    plt.clf()

    # create the grid
    nx = 65
    ng = 1
    g = FDGrid(nx, ng)


    # define the CFL and speed
    C = 0.9
    u = 1.0

    # time info
    tmax = 5.0*(g.xmax - g.xmin)/u

    # initialize
    g.init_cond("sine")

    # evolve
    evolve_upwind(g, C, u, tmax)


    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":")
    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1])

    plt.xlabel(r"$x$")
    plt.ylabel(r"$a$")

    plt.savefig("fd-upwind-sine-5T.png")
