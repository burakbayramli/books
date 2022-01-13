#!/usr/bin/env python

"""

an example of using the multigrid class to solve Laplace's equation.  Here, we
solve

u_xx = sin(x)
u = 0 on the boundary [0,1]

The analytic solution is u(x) = -sin(x) + x sin(1)

"""
#from io import *
import numpy
import multigrid
import pylab

# the analytic solution
def true(x):
    return -numpy.sin(x) + x*numpy.sin(1.0)


# the L2 error norm
def error(myg, r):

    # L2 norm of elements in r, multiplied by dx to
    # normalize
    return numpy.sqrt(myg.dx*numpy.sum((r[myg.ilo:myg.ihi+1]**2)))


# the righthand side
def f(x):
    return numpy.sin(x)

                


def smoothRun(nx):

    # create the multigrid object
    a = multigrid.ccMG1d(nx,
                         xlBCtype="dirichlet", xrBCtype="dirichlet",
                         verbose=0)

    # initialize the solution to 0
    a.initSolution(a.solnGrid.scratchArray())

    # initialize the RHS using the function f
    a.initRHS(f(a.x))

    # smooth 
    n = numpy.arange(20000) + 1
    e = []
    r = []

    for i in n:

        # do 1 smoothing at the finest level
        a.smooth(a.nlevels-1,1)

        # compute the true error (wrt the analytic solution)
        v = a.getSolution()
        e.append(error(a.solnGrid, v - true(a.x)))
        
        # compute the residual
        a.computeResidual(a.nlevels-1)
        r.append(error(a.solnGrid, a.grids[a.nlevels-1].getVarPtr("r")) )


    r = numpy.array(r)
    e = numpy.array(e)

    return n, r, e


# test the multigrid solver
N = [16, 32, 64]

c = ["r", "g", "b"]

for nx in N:

    n, r, e = smoothRun(nx)
    color = c.pop()
    pylab.plot(n, e, color=color, label = `nx`)
    pylab.plot(n, r, color=color, ls=":")

ax = pylab.gca()
ax.set_xscale('log')
ax.set_yscale('log')

pylab.legend(frameon=False)

pylab.savefig("smooth-error.png")


