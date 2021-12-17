# finite-difference implementation of FTCS for linear advection.
#
# We are solving a_t + u a_x = 0
#
# The FTCS discretization is: anew = aold + (C/2) (aold_{i+1} - aold_{i-1})
#
# where C is the CFL number
#
# M. Zingale (2013-03-12)

import numpy as np
import matplotlib.pyplot as plt
import sys

class FDGrid(object):

    def __init__(self, nx, ng, xmin=0.0, xmax=1.0):

        self.xmin = xmin
        self.xmax = xmax
        self.ng = ng
        self.nx = nx

        # python is zero-based.  Make easy integers to know where the
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

        self.a[:] = 0.0
        if type == "tophat":
            self.a[np.logical_and(self.x >= 0.333, self.x <= 0.666)] = 1.0
        elif type == "sine":
            self.a[:] = np.sin(2.0*np.pi*self.x/(self.xmax-self.xmin))
        else:
            print("unknown type")

        self.ainit = self.a.copy()


def solve_advection(g, u, C, method="upwind",
                    init_cond="tophat", num_periods=1.0):

    # time info
    dt = C*g.dx/u
    t = 0.0
    tmax = num_periods*(g.xmax - g.xmin)/u


    # initialize the data -- tophat
    g.init_cond(type=init_cond)

    # evolution loop
    anew = g.scratch_array()

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

            if method == "upwind":
                anew[i] = g.a[i] - C*(g.a[i] - g.a[i-1])

            elif method == "FTCS":
                anew[i] = g.a[i] - 0.5*C*(g.a[i+1] - g.a[i-1])

            elif method == "downwind":
                anew[i] = g.a[i] - C*(g.a[i+1] - g.a[i])

            elif method == "LF":
                anew[i] = 0.5*(1 + C)*g.a[i-1] + 0.5*(1 - C)*g.a[i+1]

            else:
                sys.exit("invalid method")

        # store the updated solution
        g.a[:] = anew[:]

        t += dt


def main():

    # create the grid
    nx = 65
    ng = 1
    g = FDGrid(nx, ng)


    # define the CFL and speed
    Clist = [0.1, 0.5, 0.9, 1.5]
    u = 1.0

    for n, C in enumerate(Clist):
        solve_advection(g, u, C, method="upwind", init_cond="tophat")

        if n == 0:
            plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":",
                     label="exact")

        plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1],
                 label=r"$C = {}$".format(C))

    plt.xlabel(r"$x$", fontsize=16)
    plt.ylabel(r"$a$", fontsize=16)

    plt.legend(frameon=False, loc="best")

    plt.tight_layout()

    plt.savefig("fdadvect.png")


if __name__ == "__main__":
    main()
