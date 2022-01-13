# 2nd-order accurate finite-volume implementation of the inviscid Burger's equation
# with piecewise linear slope reconstruction
# 
# We are solving u_t + u u_x = 0 with outflow boundary conditions
#
# M. Zingale (2013-03-26)

import numpy
import pylab
import math
import sys

class ccFVgrid:

    def __init__(self, nx, ng, xmin=0.0, xmax=1.0):

        self.xmin = xmin
        self.xmax = xmax
        self.ng = ng
        self.nx = nx

        # python is zero-based.  Make easy intergers to know where the
        # real data lives
        self.ilo = ng
        self.ihi = ng+nx-1

        # physical coords -- cell-centered, left and right edges
        self.dx = (xmax - xmin)/(nx)
        self.x = xmin + (numpy.arange(nx+2*ng)-ng+0.5)*self.dx
        self.xl = xmin + (numpy.arange(nx+2*ng)-ng)*self.dx
        self.xr = xmin + (numpy.arange(nx+2*ng)-ng+1.0)*self.dx

        # storage for the solution
        self.u = numpy.zeros((nx+2*ng), dtype=numpy.float64)

    def period(self, u):
        """ return the period for advection with velocity u """
        return (self.xmax - self.xmin)/u

    def scratchArray(self):
        """ return a scratch array dimensioned for our grid """
        return numpy.zeros((self.nx+2*self.ng), dtype=numpy.float64)

    def fillBCs(self):
        """ fill all ghostcells with outflow """

        # left boundary
        self.u[0:self.ilo] = self.u[self.ilo]

        # right boundary
        self.u[self.ihi+1:] = self.u[self.ihi]

    def initCond(self, type="tophat"):

        if type == "tophat":
            self.u[numpy.logical_and(self.x >= 0.333, self.x <= 0.666)] = 1.0

        elif type == "sine":
            self.u[:] = 1.0

            index = numpy.logical_and(self.x >= 0.333, self.x <= 0.666)
            self.u[index] += 0.5*numpy.sin(2.0*math.pi*(self.x[index]-0.333)/0.333)

        elif type == "rarefaction":
            self.u[:] = 1.0 
            self.u[self.x > 0.5] = 2.0

        self.uinit = self.u.copy()

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return numpy.sqrt(self.dx*numpy.sum(e[self.ilo:self.ihi+1]**2))



#-----------------------------------------------------------------------------
# advection-specific routines

def timestep(g, C):
    return C*g.dx/max(abs(g.u[g.ilo:g.ihi+1]))


def states(g, dt, slopeType):
    """ compute the left and right interface states """

    # compute the piecewise linear slopes
    slope = g.scratchArray()


    if slopeType == "godunov":

        # piecewise constant = 0 slopes
        slope[:] = 0.0

    elif slopeType == "centered":

        # unlimited centered difference slopes

        i = g.ilo-1
        while (i <= g.ihi+1):
            slope[i] = 0.5*(g.u[i+1] - g.u[i-1])/g.dx
            i += 1

    elif slopeType == "minmod":

        # minmod limited slope

        i = g.ilo-1
        while (i <= g.ihi+1):
            slope[i] = minmod( (g.u[i] - g.u[i-1])/g.dx, 
                               (g.u[i+1] - g.u[i])/g.dx )
            i += 1
        
    elif slopeType == "MC":

        # MC limiter

        i = g.ilo-1
        while (i <= g.ihi+1):
            slope[i] = minmod(minmod( 2.0*(g.u[i] - g.u[i-1])/g.dx, 
                                      2.0*(g.u[i+1] - g.u[i])/g.dx ),
                              0.5*(g.u[i+1] - g.u[i-1])/g.dx)
            i += 1

    elif slopeType == "superbee":

        # superbee limiter

        i = g.ilo-1
        while (i <= g.ihi+1):
            A = minmod( (g.u[i+1] - g.u[i])/g.dx,
                        2.0*(g.u[i] - g.u[i-1])/g.dx )

            B = minmod( (g.u[i] - g.u[i-1])/g.dx,
                        2.0*(g.u[i+1] - g.u[i])/g.dx )
            
            slope[i] = maxmod(A, B)
            i += 1



    # loop over all the interfaces.  Here, i refers to the left
    # interface of the zone.  Note that thre are 1 more interfaces
    # than zones
    ul = g.scratchArray()
    ur = g.scratchArray()

    i = g.ilo
    while (i <= g.ihi+1):

        # left state on the current interface comes from zone i-1
        ul[i] = g.u[i-1] + 0.5*g.dx*(1.0 - g.u[i-1]*dt/g.dx)*slope[i-1]

        # right state on the current interface comes from zone i
        ur[i] = g.u[i] - 0.5*g.dx*(1.0 + g.u[i]*dt/g.dx)*slope[i]

        i += 1

    return ul, ur


def riemann(g, ul, ur):
    """ Riemann problem for advection -- this is simply upwinding,
        but we return the flux """

    f = g.scratchArray()

    i = g.ilo
    while (i <= g.ihi+1):

        if ul[i] > ur[i]:

            # shock
            S = 0.5*(ul[i] + ur[i])

            if (S > 0):
                us = ul[i]
            elif (S < 0):
                us = ur[i]
            else:
                f[i] = 0.0


        else:
            
            # rarefaction
            if ul[i] >= 0.0:
                us = ul[i]
            elif ur[i] <= 0.0:
                us = ur[i]
            else:
                f[i] = 0.0

        f[i] = 0.5*us*us

        i += 1
    
    return f


def update(g, dt, flux):
    """ conservative update """

    anew = g.scratchArray()

    anew[g.ilo:g.ihi+1] = g.u[g.ilo:g.ihi+1] + \
        dt/g.dx * (flux[g.ilo:g.ihi+1] - flux[g.ilo+1:g.ihi+2])

    return anew


def evolve(nx, C, u, numPeriods, ICname, slopeType="centered"):

    ng = 2

    # create the grid
    g = ccFVgrid(nx, ng)

    t = 0.0
    tmax = numPeriods*g.period(u)

    # initialize the data
    g.initCond(ICname)


    # main evolution loop
    while (t < tmax):

        # fill the boundary conditions
        g.fillBCs()

        # get the timestep
        dt = timestep(g, C)

        if (t + dt > tmax):
            dt = tmax - t

        # get the interface states
        ul, ur = states(g, dt, slopeType)

        # solve the Riemann problem at all interfaces
        flux = riemann(g, ul, ur)
        
        # do the conservative update
        unew = update(g, dt, flux)

        g.u[:] = unew[:]

        t += dt


    return g


def minmod(a, b):
    if (abs(a) < abs(b) and a*b > 0.0):
        return a
    elif (abs(b) < abs(a) and a*b > 0.0):
        return b
    else:
        return 0.0

def maxmod(a, b):
    if (abs(a) > abs(b) and a*b > 0.0):
        return a
    elif (abs(b) > abs(a) and a*b > 0.0):
        return b
    else:
        return 0.0



#-----------------------------------------------------------------------------
# sine

u = 1.0
nx = 128
C = 0.8

pylab.clf()

for i in range(0,10):
    tend = (i+1)*0.02
    g = evolve(nx, C, u, tend, "sine", slopeType="MC")

    c = 1.0 - (0.1 + i*0.1)
    pylab.plot(g.x[g.ilo:g.ihi+1], g.u[g.ilo:g.ihi+1], color=`c`)


pylab.plot(g.x[g.ilo:g.ihi+1], g.uinit[g.ilo:g.ihi+1], ls=":", color="0.5")

pylab.savefig("fv-burger-sine.png")


#-----------------------------------------------------------------------------
# rarefaction

u = 1.0
nx = 128
C = 0.8

pylab.clf()

for i in range(0,10):
    tend = (i+1)*0.02

    g = evolve(nx, C, u, tend, "rarefaction", slopeType="MC")

    c = 1.0 - (0.1 + i*0.1)
    pylab.plot(g.x[g.ilo:g.ihi+1], g.u[g.ilo:g.ihi+1], color=`c`)


pylab.plot(g.x[g.ilo:g.ihi+1], g.uinit[g.ilo:g.ihi+1], ls=":", color="0.5")

pylab.savefig("fv-burger-rarefaction.png")

