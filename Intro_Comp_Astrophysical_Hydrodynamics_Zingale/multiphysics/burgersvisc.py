"""
solve the viscous Burger's equation: u_t + u u_x = eps u_xx

we use an explicit piecewise linear finite-volume method to evaluate
the advective flux and then discretize the diffusion part implicitly
(Crank-Nicolson) with the advective piece as a source to update in
time.

The resulting method is second-order in space and time.

M. Zingale
"""

import numpy as np
from scipy import linalg
import sys
import matplotlib.pyplot as plt

class Grid1d(object):

    def __init__(self, nx, ng=1, xmin=0.0, xmax=1.0, vars=None):
        """ grid class initialization """
        
        self.nx = nx
        self.ng = ng

        self.xmin = xmin
        self.xmax = xmax

        self.ilo = ng
        self.ihi = ng+nx-1

        self.dx = (xmax - xmin)/nx
        self.x = (np.arange(nx+2*ng) + 0.5 - ng)*self.dx + xmin

        self.data = {}

        for v in vars:
            self.data[v] = np.zeros((2*ng+nx), dtype=np.float64)


    def scratch_array(self):
        return np.zeros((2*self.ng+self.nx), dtype=np.float64)


    def norm(self, e):
        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))

        
    def fill_BCs(self, var):

        if not var in self.data.keys():
            sys.exit("invalid variable")

        vp = self.data[var]

        # Neumann BCs
        vp[0:self.ilo+1] = vp[self.ilo]
        vp[self.ihi+1:] = vp[self.ihi]

    def restrict(self, fac=2):
        """ restrict the data q that lives on this grid by a 
        factor fac and return the new data"""
        
        # we require fac to be a multiple of 2

        gnew = Grid1d(self.nx//fac, ng=self.ng, 
                      xmin=self.xmin, xmax=self.xmax, vars=self.data.keys())

        for v in self.data:
            b = self.data[v][self.ilo:self.ihi+1]
            gnew.data[v][gnew.ilo:gnew.ihi+1] = \
                        np.mean(b.reshape(-1, fac), axis=1)
        return gnew


class Simulation(object):

    def __init__(self, grid):
        self.grid = grid
        self.t = 0.0

    def init_cond(self):
        """ initial conditions -- a smoothed tophat """

        g = self.grid
        u = g.data["u"]

        index = np.logical_and(g.x >= 0.333, g.x <= 0.666)
        u[:] = 1.0
        u[index] += 0.5*np.sin(2.0*np.pi*(g.x[index]-0.333)/0.333)

    def timestep(self, C):
        """ estimate the timestep -- crossing time of a zone """
        u = self.grid.data["u"]
        dt = C*self.grid.dx/max(abs(u[self.grid.ilo:self.grid.ihi+1]))
        return dt

    def diffuse(self, eps, S, dt):
        """ diffuse u implicitly (C-N) through timestep dt with source S """

        gr = self.grid
        u = gr.data["u"]

        unew = gr.scratch_array()
    
        alpha = eps*dt/gr.dx**2

        # create the RHS of the matrix
        R = 0.5*eps*dt*self.lap()
        R = R[gr.ilo:gr.ihi+1]
        R += u[gr.ilo:gr.ihi+1]
        R += dt*S[gr.ilo:gr.ihi+1]
    
        # create the diagonal, d+1 and d-1 parts of the matrix
        d = (1.0 + alpha)*np.ones(gr.nx)
        u = -0.5*alpha*np.ones(gr.nx)
        u[0] = 0.0

        l = -0.5*alpha*np.ones(gr.nx)
        l[gr.nx-1] = 0.0

        # set the boundary conditions by changing the matrix elements

        # homogeneous neumann
        d[0] = 1.0 + 0.5*alpha
        d[gr.nx-1] = 1.0 + 0.5*alpha

        # dirichlet
        #d[0] = 1.0 + 1.5*alpha
        #R[0] += alpha*0.0

        #d[gr.nx-1] = 1.0 + 1.5*alpha
        #R[gr.nx-1] += alpha*0.0

        # solve
        A = np.matrix([u,d,l])
        unew[gr.ilo:gr.ihi+1] = linalg.solve_banded((1,1), A, R)

        return unew

    def advect(self, S, dt, limit=1):
        """ compute the advective term that updates u in time.  Here, S is
        a source term """

        gr = self.grid
        u = gr.data["u"]

        # we need the states in the first ghost cell too
        ib = gr.ilo-1
        ie = gr.ihi+1

        # compute the limited slopes -- 2nd order MC limiter
        test = gr.scratch_array()
        test[ib:ie+1] = (u[ib+1:ie+2] - u[ib:ie+1])*(u[ib:ie+1] - u[ib-1:ie])

        dc = gr.scratch_array()
        dl = gr.scratch_array()
        dr = gr.scratch_array()

        dc[ib:ie+1] = 0.5*(u[ib+1:ie+2] - u[ib-1:ie ])
        dl[ib:ie+1] = np.fabs(u[ib+1:ie+2] - u[ib  :ie+1])
        dr[ib:ie+1] = np.fabs(u[ib  :ie+1] - u[ib-1:ie ])
        
        if limit:
            minslope = np.minimum(np.fabs(dc), np.minimum(2.0*dl, 2.0*dr))
            ldeltau = np.where(test > 0.0, minslope, 0.0)*np.sign(dc)
        else:
            ldeltau = dc

        # construct the interface states, to second order in space and
        # time
        ul = gr.scratch_array()
        ur = gr.scratch_array()

        ur[ib:ie+1] = u[ib:ie+1] - \
            0.5*(1.0 + u[ib:ie+1]*dt/gr.dx)*ldeltau[ib:ie+1] + 0.5*dt*S[ib:ie+1]

        ul[ib+1:ie+2] = u[ib:ie+1] + \
            0.5*(1.0 - u[ib:ie+1]*dt/gr.dx)*ldeltau[ib:ie+1] + 0.5*dt*S[ib:ie+1]

        # Riemann problem -- Burger's Eq

        # shock speed and shock state
        S = 0.5*(ul + ur)
        ushock = np.where(S > 0.0, ul, ur)
        ushock = np.where(S == 0.0, 0.0, ushock)

        # rarefaction solution
        urare = np.where(ur <= 0.0, ur, 0.0)
        urare = np.where(ul >= 0.0, ul, urare)
        
        us = np.where(ul > ur, ushock, urare)

        # construct the advective update
        F = 0.5*us*us
        A = gr.scratch_array()

        A[ib:ie+1] = (F[ib:ie+1] - F[ib+1:ie+2])/gr.dx

        return A

    def lap(self):
        """ compute the Laplacian of u, including the first ghost cells """

        gr = self.grid
        u = gr.data["u"]

        lapu = gr.scratch_array()

        ib = gr.ilo-1
        ie = gr.ihi+1

        lapu[ib:ie+1] = (u[ib-1:ie] - 2.0*u[ib:ie+1] + u[ib+1:ie+2])/gr.dx**2

        return lapu

    def evolve(self, eps, cfl, tmax, limit=1, dovis=0):
        """ 
        the main evolution loop.  Evolve 
  
        phi_t + u u_x = eps u_xx

        from t = 0 to tmax
        """

        gr = self.grid

        # pointers to the data at various stages
        u  = gr.data["u"]

        gr.fill_BCs("u")

        # runtime plotting
        if dovis == 1:
            plt.ion()
    
        self.t = 0.0
        while self.t < tmax:

            dt = self.timestep(cfl)

            if (self.t + dt > tmax):
                dt = tmax - self.t

            # construct the explicit diffusion source
            S = eps*self.lap()

            # construct the advective update
            A = self.advect(S, dt, limit=limit)

            # diffuse for dt
            unew = self.diffuse(eps, A, dt)
            
            # to just do advection, we'd do this:
            #unew = u + dt*A

            u[:] = unew[:]
            gr.fill_BCs("u")

            self.t += dt

            if dovis == 1:
                plt.clf()
                plt.plot(gr.x, u)
                plt.xlim(gr.xmin,gr.xmax)
                plt.ylim(0.0,2.0)
                plt.title("t = %f" % (t))
                plt.draw()



if __name__ == "__main__":
    nx = 256
    cfl = 0.8
    tmax = 0.2

    g1 = Grid1d(nx, ng=2, vars=["u"])
    g2 = Grid1d(nx, ng=2, vars=["u"])
    g3 = Grid1d(nx, ng=2, vars=["u"])

    eps1 = 0.005
    s1 = Simulation(g1)
    s1.init_cond()
    s1.evolve(eps1, cfl, tmax, dovis=0)

    eps2 = 0.0005
    s2 = Simulation(g2)
    s2.init_cond()
    s2.evolve(eps2, cfl, tmax, dovis=0)

    eps3 = 0.00005
    s3 = Simulation(g3)
    s3.init_cond()
    s3.evolve(eps3, cfl, tmax, dovis=0)

    u1 = s1.grid.data["u"]
    plt.plot(g1.x, u1, label=r"$\epsilon = %f$" % (eps1))

    u2 = s2.grid.data["u"]
    plt.plot(g2.x, u2, label=r"$\epsilon = %f$" % (eps2), ls="--", color="k")

    u3 = s3.grid.data["u"]
    plt.plot(g3.x, u3, label=r"$\epsilon = %f$" % (eps3), ls=":", color="k")

    plt.legend(frameon=False)

    plt.xlim(0.0, 1.0)

    plt.tight_layout()
    plt.savefig("burgersvisc.pdf")
