# first-order finite-difference implicit method for linear advection
#
# We are solving a_t + u a_x = 0
#
# The upwinded implicit update appears as: 
#
#      n+1             n+1    n
#  -C a     + (1 - C) a    = a
#      i-1             i      i
# 
# where C is the CFL number
#
# We use a periodic grid with N points, 0, ..., N-1, with the data
# located at those points.  This means that since 0 and N-1 are on
# the boundary, they are the same point.  Therefore, we only need
# to update points 1, ..., N-1
#
# No ghost points are used here.

import numpy as np
import matplotlib.pyplot as plt


import matplotlib as mpl

mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'

class FDgrid:

    def __init__(self, nx, xmin=0.0, xmax=1.0):

        self.xmin = xmin
        self.xmax = xmax
        self.nx = nx

        # python is zero-based.  We are assuming periodic BCs, so
        # points 0 and N-1 are the same.  Set some integer indices to
        # allow us to easily access points 1 through N-1.  Point 0
        # won't be explicitly updated, but rather filled by the BC
        # routine.
        self.ilo = 1
        self.ihi = nx-1

        # physical coords
        self.dx = (xmax - xmin)/(nx-1)
        self.x = xmin + np.arange(nx)*self.dx

        # storage for the solution
        self.a = np.zeros((nx), dtype=np.float64)
        self.ainit = np.zeros((nx), dtype=np.float64)

    def scratchArray(self):
        """ return a scratch array dimensioned for our grid """
        return np.zeros((self.nx), dtype=np.float64)

    def fillBCs(self):
        """ we don't explicitly update point 0, since it is identical
            to N-1, so fill it here """
        self.a[0] = self.a[self.ihi]


def evolve(nx, C, u, tmax):

    # create the grid
    g = FDgrid(nx)

    # time info
    dt = C*g.dx/u
    t = 0.0

    # initialize the data -- tophat
    g.a[np.logical_and(g.x >= 0.333, g.x <= 0.666)] = 1.0

    g.ainit = g.a.copy()

    # evolution loop
    A = np.zeros((g.nx-1, g.nx-1), dtype=np.float64)

    # fill the boundary conditions
    g.fillBCs()

    while t < tmax:

        # create the matrix
    
        # loop over rows [ilo,ihi] and construct the matrix.  This will
        # be almost bidiagonal, but with the upper right entry also
        # nonzero.
        for i in range(g.nx-1):
            A[i,i] = 1.0 + C
            A[i,i-1] = -C


        # create the RHS -- this holds all entries except for a[0]
        b = g.a[g.ilo:g.ihi+1]

        # solve the system
        anew = np.linalg.solve(A, b)

        g.a[g.ilo:g.ihi+1] = anew[:]
    
        g.fillBCs()


        t += dt

    return g




u = 1.0
tmax = 1.0/u

nx = 65
CFL = [0.5, 1.0, 10.0]

for n, C in enumerate(CFL):

    g = evolve(nx, C, u, tmax)

    if n == 0:
        plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", label="exact")

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], label="$C = %3.1f$" % (C))



#plt.title("N = %d" % (nx))
plt.xlabel("$x$", fontsize=16)
plt.ylabel("$a$", fontsize=16)


plt.legend(frameon=False, loc="best")

plt.tight_layout()

plt.savefig("fdadvect-implicit.pdf")
