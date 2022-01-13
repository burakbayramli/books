# 2nd-order accurate finite-volume implementation of linear advection with
# piecewise linear slope reconstruction
#
# We are solving a_t + u a_x = 0
#
# M. Zingale (2013-03-24)

import numpy as np
import matplotlib.pyplot as plt

class FVGrid:

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
        self.x = xmin + (np.arange(nx+2*ng)-ng+0.5)*self.dx
        self.xl = xmin + (np.arange(nx+2*ng)-ng)*self.dx
        self.xr = xmin + (np.arange(nx+2*ng)-ng+1.0)*self.dx

        # storage for the solution
        self.a = np.zeros((nx+2*ng), dtype=np.float64)

    def period(self, u):
        """ return the period for advection with velocity u """
        return (self.xmax - self.xmin)/u

    def scratch_array(self):
        """ return a scratch array dimensioned for our grid """
        return np.zeros((self.nx+2*self.ng), dtype=np.float64)

    def fill_BCs(self):
        """ fill all single ghostcell with periodic boundary conditions """

        # left boundary
        for n in range(self.ng):
            self.a[self.ilo-1-n] = self.a[self.ihi-n]

        # right boundary
        for n in range(self.ng):
            self.a[self.ihi+1+n] = self.a[self.ilo+n]

    def init_cond(self, type="tophat"):

        if type == "tophat":
            self.a[np.logical_and(self.x >= 0.333, self.x <= 0.666)] = 1.0
        elif type == "sine":
            self.a[:] = np.sin(2.0*np.pi*self.x/(self.xmax-self.xmin))
        elif type == "gaussian":
            self.a[:] = 1.0 + np.exp(-60.0*(self.x - 0.5)**2)

        self.ainit = self.a.copy()

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))



#-----------------------------------------------------------------------------
# advection-specific routines

def timestep(g, C, u):
    return C*g.dx/u


def states(g, dt, u, slope_type):
    """ compute the left and right interface states """

    # compute the piecewise linear slopes
    slope = g.scratch_array()

    if slope_type == "godunov":

        # piecewise constant = 0 slopes
        slope[:] = 0.0

    elif slope_type == "centered":

        # unlimited centered difference slopes
        for i in range(g.ilo-1, g.ihi+2):
            slope[i] = 0.5*(g.a[i+1] - g.a[i-1])/g.dx

    elif slope_type == "minmod":

        # minmod limited slope
        for i in range(g.ilo-1, g.ihi+2):
            slope[i] = minmod( (g.a[i] - g.a[i-1])/g.dx,
                               (g.a[i+1] - g.a[i])/g.dx )

    elif slope_type == "MC":

        # MC limiter
        for i in range(g.ilo-1, g.ihi+2):
            slope[i] = minmod(minmod( 2.0*(g.a[i] - g.a[i-1])/g.dx,
                                      2.0*(g.a[i+1] - g.a[i])/g.dx ),
                              0.5*(g.a[i+1] - g.a[i-1])/g.dx)

    elif slope_type == "superbee":

        # superbee limiter
        for i in range(g.ilo-1, g.ihi+2):
            A = minmod( (g.a[i+1] - g.a[i])/g.dx,
                        2.0*(g.a[i] - g.a[i-1])/g.dx )

            B = minmod( (g.a[i] - g.a[i-1])/g.dx,
                        2.0*(g.a[i+1] - g.a[i])/g.dx )

            slope[i] = maxmod(A, B)


    # loop over all the interfaces.  Here, i refers to the left
    # interface of the zone.  Note that thre are 1 more interfaces
    # than zones
    al = g.scratch_array()
    ar = g.scratch_array()

    for i in range(g.ilo, g.ihi+2):

        # left state on the current interface comes from zone i-1
        al[i] = g.a[i-1] + 0.5*g.dx*(1.0 - u*dt/g.dx)*slope[i-1]

        # right state on the current interface comes from zone i
        ar[i] = g.a[i] - 0.5*g.dx*(1.0 + u*dt/g.dx)*slope[i]

    return al, ar


def riemann(u, al, ar):
    """ Riemann problem for advection -- this is simply upwinding,
        but we return the flux """

    if u > 0.0:
        return u*al
    else:
        return u*ar


def update(g, dt, flux):
    """ conservative update """

    anew = g.scratch_array()

    anew[g.ilo:g.ihi+1] = g.a[g.ilo:g.ihi+1] + \
        dt/g.dx * (flux[g.ilo:g.ihi+1] - flux[g.ilo+1:g.ihi+2])

    return anew


def evolve(nx, C, u, num_periods, init_cond_name, slope_type="centered"):

    ng = 2

    # create the grid
    g = FVGrid(nx, ng)

    t = 0.0
    tmax = num_periods*g.period(u)

    # initialize the data
    g.init_cond(init_cond_name)


    # main evolution loop
    while t < tmax:

        # fill the boundary conditions
        g.fill_BCs()

        # get the timestep
        dt = timestep(g, C, u)

        if t + dt > tmax:
            dt = tmax - t

        # get the interface states
        al, ar = states(g, dt, u, slope_type)

        # solve the Riemann problem at all interfaces
        flux = riemann(u, al, ar)

        # do the conservative update
        anew = update(g, dt, flux)

        g.a[:] = anew[:]

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
# tophat

u = 1.0
nx = 64
C = 0.8

g = evolve(nx, C, u, 5, "tophat")

plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], color="r")
plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", color="0.5")

plt.savefig("fv-advect-tophat.png")


#-----------------------------------------------------------------------------
# gaussian

u = 1.0
nx = 64
C = 0.8

g = evolve(nx, C, u, 5, "gaussian")

plt.clf()

plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], color="r")
plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", color="0.5")

plt.savefig("fv-advect-gaussian.png")



#-----------------------------------------------------------------------------
# convergence test
problem = "gaussian"
N = [32, 64, 128, 256]
u = 1.0
C = 0.8

err = []

for nx in N:
    g = evolve(nx, C, u, 5, problem)

    err.append(g.norm(g.a - g.ainit))
    print(g.dx, nx, err[-1])

plt.clf()

N = np.array(N, dtype=np.float64)
err = np.array(err)

plt.scatter(N, err, color="r")
plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="k", label="2nd order convergence")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel("N")
plt.ylabel("absolute error")

plt.legend(frameon=False, fontsize="small")

plt.savefig("plm-converge.png")


#-----------------------------------------------------------------------------
# gaussian -- different limiters

u = 1.0
nx = 128
C = 0.8

limiters = ["godunov", "centered", "minmod", "MC", "superbee"]

for l in limiters:
    g = evolve(nx, C, u, 5, "gaussian", slope_type=l)

    plt.clf()

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], color="r")
    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", color="0.5")

    plt.title("limiter: {}".format(l))

    f = plt.gcf()
    f.set_size_inches(6.0,7.0)

    plt.savefig("fv-gaussian-{}.png".format(l), bbox_inches="tight")


#-----------------------------------------------------------------------------
# tophat -- different limiters

u = 1.0
nx = 128
C = 0.8

for l in limiters:
    g = evolve(nx, C, u, 5, "tophat", slope_type=l)

    plt.clf()

    plt.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1], color="r")
    plt.plot(g.x[g.ilo:g.ihi+1], g.ainit[g.ilo:g.ihi+1], ls=":", color="0.5")

    plt.title("limiter: {}".format(l))

    f = plt.gcf()
    f.set_size_inches(6.0,7.0)

    plt.savefig("fv-tophat-{}.png".format(l), bbox_inches="tight")
