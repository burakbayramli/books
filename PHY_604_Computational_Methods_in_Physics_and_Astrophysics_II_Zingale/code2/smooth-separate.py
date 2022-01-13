#!/usr/bin/env python

"""

an example of solving Poisson's equation via smoothing only.  Here, we
solve

u_xx = sin(x)
u = 0 on the boundary [0,1]

The analytic solution is u(x) = -sin(x) + x sin(1)

This version (separate) differs from smooth.py in that we implement the
smoothing here directly, instead of using the MG solver.

M. Zingale (2013-03-31)

"""
#from io import *
import numpy as np
import matplotlib.pyplot as plt
import sys

def true(x):
    # the analytic solution
    return -np.sin(x) + x*np.sin(1.0)


def error(ilo, ihi, dx, r):
    # L2 norm of elements in r, multiplied by dx to normalize
    return np.sqrt(dx*np.sum((r[ilo:ihi+1]**2)))


def f(x):
    # the righthand side
    return np.sin(x)


def compute_residual(ilo, ihi, dx, phi, frhs):
    # compute r = f - L phi
    r = np.zeros(len(phi))
    r[ilo:ihi+1] = frhs[ilo:ihi+1] - \
        (phi[ilo+1:ihi+2] - 2.0*phi[ilo:ihi+1] + phi[ilo-1:ihi])/dx**2
    return r


def smooth_run(nx, method="GS"):

    xmin = 0.0
    xmax = 1.0

    ng = 1

    # initialize the solution to zero.  Put one ghost cell on either end
    phi = np.zeros(nx + 2*ng, dtype=np.float64)
    phinew = np.zeros_like(phi)

    ilo = ng
    ihi = ng + nx - 1

    # coordinates of centers
    dx = (xmax - xmin)/nx
    x = (np.arange(nx+2*ng) - ng + 0.5)*dx + xmin

    # initialize the RHS using the function f
    frhs = f(x)

    # smooth 
    n = np.arange(20000) + 1
    e = []
    r = []

    print("source norm: ", error(ilo, ihi, dx, frhs))
    print(np.sum(frhs[ilo:ihi+1]))

    for i in n:

        # fill the ghost cells
        phi[ilo-1] = -phi[ilo]
        phi[ihi+1] = -phi[ihi]

        if method == "Jacobi":
            phinew[ilo:ihi+1] = \
                (-dx*dx*frhs[ilo:ihi+1] + phi[ilo+1:ihi+2] + phi[ilo-1:ihi])/2.0
            phi[:] = phinew[:]

        elif method == "GS":

            # red-black Gauss-Seidel -- first do the odd, then even points
            phi[ilo:ihi+1:2] = \
                0.5*(-dx*dx*frhs[ilo:ihi+1:2] + 
                     phi[ilo+1:ihi+2:2] + phi[ilo-1:ihi:2])

            # fill the ghost cells between red and black
            phi[ilo-1] = -phi[ilo]
            phi[ihi+1] = -phi[ihi]

            phi[ilo+1:ihi+1:2] = \
                0.5*(-dx*dx*frhs[ilo+1:ihi+1:2] + \
                     phi[ilo+2:ihi+2:2] + phi[ilo:ihi:2])

        else:
            sys.exit("invalid method")

        # compute the true error (wrt the analytic solution) and residual
        e.append(error(ilo, ihi, dx, phi - true(x)))
        
        # compute the residual
        resid = compute_residual(ilo, ihi, dx, phi, frhs)
        r.append(error(ilo, ihi, dx, resid))

    return n, np.array(r), np.array(e)


# test the multigrid solver
N = [16, 32, 64]

c = ["r", "g", "b"]

for nx in N:

    n, r, e = smooth_run(nx)
    color = c.pop()
    plt.plot(n, e, color=color, label = str(nx))
    plt.plot(n, r, color=color, ls=":")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel("# of iterations")
plt.ylabel("L2 norm of true error (solid) and residual (dotted)")
plt.legend(frameon=False, fontsize="small")

plt.savefig("smooth-error.png")


