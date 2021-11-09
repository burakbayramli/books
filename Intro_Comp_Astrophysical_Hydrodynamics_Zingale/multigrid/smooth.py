#!/usr/bin/env python

from __future__ import print_function

"""

an example of using the multigrid class to solve Poisson's equation via
smoothing only.  Here, we solve

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


# the L2 error norm
def error(myg, r):

    # L2 norm of elements in r, multiplied by dx to
    # normalize
    return np.sqrt(myg.dx*np.sum((r[myg.ilo:myg.ihi+1]**2)))


# the righthand side
def f(x):
    return np.sin(x)

                
def smooth_run(nx, nsmooth=20000, modes=None, return_sol=False, rhs=f):
    """
    do smoothing for a 1-d Poisson problem with nx zones on a cell-centered
    grid.

    nsmooth is the number of smoothing iterations to do

    modes (optional) is a list of equally-weighted sine modes to initialize 
    the solution with

    return_sol=True will return the solution object (CellCenteredMG1d), otherwise
    it will return arrays of the error and residual as a function of iteration

    rhs is the function to call to define the RHS of the Poisson equation.
    """

    # create the multigrid object
    a = multigrid.CellCenterMG1d(nx,
                                 xl_BC_type="dirichlet", xr_BC_type="dirichlet",
                                 verbose=0)

    if modes is None:
        # initialize the solution to 0
        a.init_zeros()
    else:
        phi = a.soln_grid.scratch_array()
        for m in modes:
            phi += np.sin(2.0*np.pi*m*a.x)
        phi /= len(modes)
        a.init_solution(phi)

    # initialize the RHS using the function f
    a.init_RHS(rhs(a.x))

    # smooth 
    n = np.arange(nsmooth) + 1
    e = []
    r = []

    for i in n:

        # do 1 smoothing at the finest level
        a.smooth(a.nlevels-1,1)

        # compute the true error (wrt the analytic solution)
        v = a.get_solution()
        e.append(error(a.soln_grid, v - true(a.x)))
        
        # compute the residual
        a._compute_residual(a.nlevels-1)
        r.append(error(a.soln_grid, a.grids[a.nlevels-1].get_var("r")) )

    r = np.array(r)
    e = np.array(e)

    print(nsmooth, r[-1])

    if return_sol:
        return a
    else:
        return n, r, e


if __name__ == "__main__":

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

    plt.tight_layout()

    plt.savefig("smooth-error.png")
    plt.savefig("smooth-error.pdf")


