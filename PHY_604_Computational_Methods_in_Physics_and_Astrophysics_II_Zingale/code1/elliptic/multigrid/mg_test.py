#!/usr/bin/env python

from __future__ import print_function

"""

an example of using the multigrid class to solve Laplace's equation.  Here, we
solve

u_xx = sin(x)
u = 0 on the boundary [0,1]

The analytic solution is u(x) = -sin(x) + x sin(1)

"""

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

                
# test the multigrid solver
nx = 256


# create the multigrid object
a = multigrid.CellCenterMG1d(nx,
                             xl_BC_type="dirichlet", xr_BC_type="dirichlet",
                             verbose=1, true_function=true)

# initialize the solution to 0
a.init_zeros()

# initialize the RHS using the function f
a.init_RHS(f(a.x))

# solve to a relative tolerance of 1.e-11
elist, rlist = a.solve(rtol=1.e-11)

Ncycle = np.arange(len(elist)) + 1


# get the solution 
v = a.get_solution()

# compute the error from the analytic solution
e = v - true(a.x)

print("L2 error from true solution = {}".format(error(a.soln_grid, e)))
print("rel. err from previous cycle = {}".format(a.relative_error))
print("num. cycles = {}".format(a.num_cycles))


plt.plot(a.x[a.ilo:a.ihi+1], true(a.x[a.ilo:a.ihi+1]), color="r")
plt.xlabel("x")
plt.ylabel(r"$\phi$")

plt.ylim([1.1*min(true(a.x[a.ilo:a.ihi+1])),0.0])
f = plt.gcf()
f.set_size_inches(10.0,4.5)

plt.savefig("phi_analytic.png")


plt.clf()

plt.plot(Ncycle, np.array(elist), color="k", label=r"$||e||$")
plt.plot(Ncycle, np.array(rlist), "--", color="k", label=r"$||r||$")

plt.xlabel("# of V-cycles")
plt.ylabel("L2 norm of error")

ax = plt.gca()
ax.set_yscale('log')

f = plt.gcf()

f.set_size_inches(8.0,6.0)

plt.legend(frameon=False)

plt.tight_layout()

plt.savefig("mg_error_vs_cycle.png")
plt.savefig("mg_error_vs_cycle.eps")


