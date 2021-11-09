#!/usr/bin/env python

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

def f(x):
    # the righthand side
    return np.sin(x)

# test the multigrid solver
nx = 128

# create the multigrid object
a = multigrid.Multigrid(nx,
                        bc_left_type="dirichlet", bc_right_type="dirichlet",
                        verbose=1, true_function=true)

# initialize the solution to 0
a.init_solution()

# initialize the RHS using the function f
a.init_rhs(f(a.x))

# solve to a relative tolerance of 1.e-11
elist, rlist = a.solve(rtol=1.e-11)

ncycle = np.arange(len(elist)) + 1


# get the solution 
v = a.get_solution()

# compute the error from the analytic solution
e = v - true(a.x)

print(f"L2 error from true solution = {a.soln_grid.norm(e)}")
print(f"rel. err from previous cycle = {a.relative_error}")
print(f"num. cycles = {a.num_cycles}")

fig = plt.figure()
ax = fig.add_subplot(111)

ax.plot(a.x[a.ilo:a.ihi+1], true(a.x[a.ilo:a.ihi+1]), color="0.5", ls=":", label="analytic solution")
ax.scatter(a.x[a.ilo:a.ihi+1], v[a.ilo:a.ihi+1], color="C1", label="multigrid solution", marker="x")
ax.set_xlabel("x")
ax.set_ylabel(r"$\phi$")

fig.set_size_inches(8.0, 8.0)

fig.savefig("phi_analytic.png")


fig = plt.figure()
ax = fig.add_subplot(111)

ax.plot(ncycle, elist, color="k", label=r"$\| e\|$")
ax.plot(ncycle, rlist, "--", color="k", label=r"$\| r\|$")

ax.set_xlabel("# of V-cycles")
ax.set_ylabel("L2 norm of error")

ax.set_yscale('log')

fig.set_size_inches(8.0,6.0)

ax.legend(frameon=False)

fig.tight_layout()

fig.savefig("mg_error_vs_cycle.png")

