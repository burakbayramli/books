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
import matplotlib as mpl

mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'

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


def solve_advection(g, u, C, method="upwind", tmax_factor=1.0):

    # time info
    dt = C*g.dx/u
    t = 0.0
    tmax = tmax_factor*(g.xmax - g.xmin)/u


    # initialize the data -- tophat
    g.a[:] = 0.0
    g.a[np.logical_and(g.x >= 1./3., g.x <= 2./3.)] = 1.0

    g.ainit = g.a.copy()

    # evolution loop
    anew = g.scratch_array()

    while t < tmax:

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
            else:
                sys.exit("invalid method")

        # store the updated solution
        g.a[:] = anew[:]
        t += dt


# create the grid
nx = 65
ng = 1
g = FDGrid(nx, ng)


# define the CFL and speed
Clist = [0.1, 0.5, 0.9]
u = 1.0

# upwind
for n, C in enumerate(Clist):
    solve_advection(g, u, C)

    if n == 0:
        plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", label="exact")

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], 
             label=r"$C = {}$".format(C))

plt.xlabel(r"$x$", fontsize=16)
plt.ylabel(r"$a$", fontsize=16)

plt.legend(frameon=False, loc="best")

plt.tight_layout()

plt.savefig("fdadvect-upwind.pdf")



plt.clf()

# upwind
for C in Clist:
    plt.clf()
    solve_advection(g, u, C, method="FTCS", tmax_factor=0.1)

    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", label="exact")

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], 
             label=r"$C = {}$".format(C))

    plt.xlabel(r"$x$", fontsize=16)
    plt.ylabel(r"$a$", fontsize=16)

    plt.legend(frameon=False, loc="best")

    plt.tight_layout()

    plt.savefig("fdadvect-FTCS-C{}.pdf".format(C))
