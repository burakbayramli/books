#!/usr/bin/env python

from __future__ import print_function

"""

an example of solving Poisson's equation via smoothing only.  Here, we
solve

u_xx = sin(x)
u = 0 on the boundary [0,1]

The analytic solution is u(x) = -sin(x) + x sin(1)

M. Zingale

"""

import numpy as np
import multigrid
import matplotlib.pyplot as plt


# the analytic solution
def true(x):
    return -np.sin(x) + x*np.sin(1.0)


# the L1 error norm
def error1(myg, r):

    # L1 norm of elements in r, multiplied by dx to
    # normalize
    return myg.dx*np.sum((abs(r[myg.ilo:myg.ihi+1])))

# the L2 error norm
def error2(myg, r):

    # L2 norm of elements in r, multiplied by dx to
    # normalize
    return np.sqrt(myg.dx*np.sum((r[myg.ilo:myg.ihi+1]**2)))

# the inf error norm
def errorinf(myg, r):

    # inf norm of elements in r
    return np.max(abs(r[myg.ilo:myg.ihi+1]))


# the righthand side
def f(x):
    return np.sin(x)

def smooth_run(nx):

    # create the multigrid object
    a = multigrid.CellCenterMG1d(nx,
                                 xl_BC_type="dirichlet", xr_BC_type="dirichlet",
                                 verbose=0)

    # initialize the solution to 0
    a.init_zeros()

    # initialize the RHS using the function f
    a.init_RHS(f(a.x))

    # smooth 
    n = np.arange(20000) + 1

    e1 = []
    r1 = []

    e2 = []
    r2 = []

    einf = []
    rinf = []

    for i in n:

        # do 1 smoothing at the finest level
        a.smooth(a.nlevels-1,1)

        # compute the true error (wrt the analytic solution)
        v = a.get_solution()
        e1.append(error1(a.soln_grid, v - true(a.x)))
        e2.append(error2(a.soln_grid, v - true(a.x)))
        einf.append(errorinf(a.soln_grid, v - true(a.x)))

        # compute the residual
        a._compute_residual(a.nlevels-1)

        r1.append(error1(a.soln_grid, a.grids[a.nlevels-1].get_var("r")) )
        r2.append(error2(a.soln_grid, a.grids[a.nlevels-1].get_var("r")) )
        rinf.append(errorinf(a.soln_grid, a.grids[a.nlevels-1].get_var("r")) )


    r1 = np.array(r1)
    e1 = np.array(e1)

    r2 = np.array(r2)
    e2 = np.array(e2)

    rinf = np.array(rinf)
    einf = np.array(einf)

    return n, r1, e1, r2, e2, rinf, einf


# test the multigrid solver
N = 64

n, r1, e1, r2, e2, rinf, einf = smooth_run(N)

print(len(n))
print(len(e1))
plt.plot(n, e1, color="r", label = "L1")
plt.plot(n, r1, color="r", ls=":")

plt.plot(n, e2, color="g", label = "L2")
plt.plot(n, r2, color="g", ls=":")

plt.plot(n, einf, color="b", label = "L-inf")
plt.plot(n, rinf, color="b", ls=":")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel("# of iterations")
plt.ylabel("Norm of true error (solid) and residual (dotted)")
plt.legend(frameon=False, fontsize="small")

plt.savefig("smooth-error-norms.pdf")


