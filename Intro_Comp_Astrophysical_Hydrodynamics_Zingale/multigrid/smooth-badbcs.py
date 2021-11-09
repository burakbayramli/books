#!/usr/bin/env python

"""

an example of solving Poisson's equation via smoothing only with the 
wrong (first-order) BCs.  

Here, we solve

u_xx = sin(x)
u = 0 on the boundary [0,1]

The analytic solution is u(x) = -sin(x) + x sin(1)

"""

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

# the analytic solution
def true(x):
    return -np.sin(x) + x*np.sin(1.0)


# the L2 error norm
def error(ilo, ihi, dx, r):

    # L2 norm of elements in r, multiplied by dx to
    # normalize
    return np.sqrt(dx*np.sum((r[ilo:ihi+1]**2)))


# the righthand side
def f(x):
    return np.sin(x)



def computeResidual(ilo, ihi, dx, phi, frhs):

    r = np.zeros(len(phi))

    r[ilo:ihi+1] = frhs[ilo:ihi+1] - \
        (phi[ilo+1:ihi+2] - 2.0*phi[ilo:ihi+1] + phi[ilo-1:ihi])/dx**2


    return r


def smoothRun(nx, badBCs=0):

    xmin = 0.0
    xmax = 1.0

    ng = 1

    # initialize the solution to zero.  Put one ghost cell on either end
    phi = np.zeros(nx + 2*ng, dtype=np.float64)
    phinew = np.zeros(nx + 2*ng, dtype=np.float64)

    ilo = ng
    ihi = ng + nx - 1

    # coordinates of centers
    dx = (xmax - xmin)/nx
    x = (np.arange(nx+2*ng) - ng + 0.5)*dx + xmin

    # initialize the RHS using the function f
    frhs = np.zeros(nx + 2*ng, dtype=np.float64)
    frhs[ilo:ihi+1] = f(x[ilo:ihi+1])

    # smooth 
    n = np.arange(20000) + 1
    e = []
    r = []

    # fill the ghost cells
    phi[ilo-1] = -phi[ilo]
    phi[ihi+1] = -phi[ihi]

    print("source norm: ", error(ilo, ihi, dx, frhs))

    for i in n:

        # red-black Gauss-Seidel -- first do the odd, then even points
        phi[ilo:ihi+1:2] = \
            0.5*(-dx*dx*frhs[ilo:ihi+1:2] + \
                      phi[ilo+1:ihi+2:2] + phi[ilo-1:ihi:2])

        # fill the ghost cells
        if (badBCs):
            phi[ilo-1] = 0.0
            phi[ihi+1] = 0.0
        else:
            phi[ilo-1] = -phi[ilo]
            phi[ihi+1] = -phi[ihi]

        phi[ilo+1:ihi+1:2] = \
            0.5*(-dx*dx*frhs[ilo+1:ihi+1:2] + \
                      phi[ilo+2:ihi+2:2] + phi[ilo:ihi:2])


        # fill the ghost cells
        if (badBCs):
            phi[ilo-1] = 0.0
            phi[ihi+1] = 0.0
        else:
            phi[ilo-1] = -phi[ilo]
            phi[ihi+1] = -phi[ihi]

        
        # compute the true error (wrt the analytic solution)
        e.append(error(ilo, ihi, dx, phi - true(x)))
        
        # compute the residual
        resid = computeResidual(ilo, ihi, dx, phi, frhs)

        r.append(error(ilo, ihi, dx, resid))


    r = np.array(r)
    e = np.array(e)

    return n, r, e


# test the multigrid solver
N = [16, 16, 32, 32, 64, 64]

c = ["r", "r:", "g", "g:", "b", "b:"]

for nx in N:

    color = c.pop()

    if len(c) % 2 == 0: 
        n, r, e = smoothRun(nx)
        plt.plot(n, e, color, label="{}".format(nx))
    else:
        n, r, e = smoothRun(nx, badBCs=1)
        plt.plot(n, e, color)


ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel("# of iterations")
plt.ylabel("L2 norm of true error:\n first-order BCs (solid); second-order BCs  (dotted)")
plt.legend(frameon=False, fontsize="small")

plt.savefig("smooth-badBCs.pdf")


