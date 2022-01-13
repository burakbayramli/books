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



def mgsolve(nx):
                
    # create the multigrid object
    a = multigrid.ccMG1d(nx, xlBCtype="dirichlet", xrBCtype="dirichlet",
                         verbose=0)

    # initialize the solution to 0
    a.initSolution(a.solnGrid.scratchArray())

    # initialize the RHS using the function f
    a.initRHS(f(a.x))

    # solve to a relative tolerance of 1.e-11
    a.solve(rtol=1.e-11)

    # get the solution 
    v = a.getSolution()

    # compute the error from the analytic solution
    return error(a.solnGrid, v - true(a.x))


N = [16, 32, 64, 128, 256, 512]
err = []

for nx in N:
    err.append(mgsolve(nx))

N = numpy.array(N, dtype=numpy.float64)
err = numpy.array(err)

pylab.scatter(N, err, color="r")
pylab.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="k", label="$\mathcal{O}(\Delta x^2)$")

print N
print err[0]*(N[0]/N)**2

ax = pylab.gca()
ax.set_xscale('log')
ax.set_yscale('log')

pylab.ylim(1.e-8, 1.e-3)

pylab.xlabel("N")
pylab.ylabel("L2 norm of absolute error")

pylab.legend(frameon=False)

pylab.savefig("mg-converge.png")




