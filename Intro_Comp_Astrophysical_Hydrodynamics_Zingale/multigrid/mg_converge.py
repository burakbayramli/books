#!/usr/bin/env python

"""
A convergence test of the multigrid solver.  We solve Laplace's equation.  Here, we
solve

u_xx = sin(x)
u = 0 on the boundary [0,1]

The analytic solution is u(x) = -sin(x) + x sin(1)

we run at a variety of resolutions and compare to the analytic solution.

"""
#from io import *
import numpy as np
import multigrid
import matplotlib.pyplot as plt


def true(x):
    # the analytic solution
    return -np.sin(x) + x*np.sin(1.0)


def error(myg, r):
    # L2 norm of elements in r, multiplied by dx to normalize
    return np.sqrt(myg.dx*np.sum((r[myg.ilo:myg.ihi+1]**2)))


def f(x):
    # the righthand side
    return np.sin(x)


def mgsolve(nx):

    # create the multigrid object
    a = multigrid.CellCenterMG1d(nx, xl_BC_type="dirichlet", xr_BC_type="dirichlet",
                                 verbose=0)

    # initialize the solution to 0
    a.init_zeros()

    # initialize the RHS using the function f
    a.init_RHS(f(a.x))

    # solve to a relative tolerance of 1.e-11
    a.solve(rtol=1.e-11)

    # get the solution
    v = a.get_solution()

    # compute the error from the analytic solution
    return error(a.soln_grid, v - true(a.x))


N = [16, 32, 64, 128, 256, 512]
err = []

for nx in N:
    err.append(mgsolve(nx))

N = np.array(N, dtype=np.float64)
err = np.array(err)

plt.scatter(N, err, color="r")
plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="k", label=r"$\mathcal{O}(\Delta x^2)$")

print(N, err[0]*(N[0]/N)**2)

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.ylim(1.e-8, 1.e-3)

plt.xlabel("N")
plt.ylabel("L2 norm of absolute error")

plt.legend(frameon=False)

plt.savefig("mg-converge.png")
