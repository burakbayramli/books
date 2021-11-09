"""
solve the diffusion equation:

 phi_t = k phi_{xx}

with a first-order (in time) implicit discretization

M. Zingale (2013-04-03)
"""

import numpy as np
from scipy import linalg
import matplotlib.pyplot as plt
from diffusion_explicit import Grid1d

class Simulation(object):

    def __init__(self, grid, k=1.0):
        self.grid = grid
        self.t = 0.0
        self.k = k  # diffusion coefficient

    def init_cond(self, name, *args):
        # initialize the data

        if name == "gaussian":
            t0, phi1, phi2 = args
            self.grid.phi[:] = self.grid.phi_a(0.0, self.k, t0, phi1, phi2)

    def diffuse_implicit(self, dt):
        """ diffuse phi implicitly through timestep dt """

        gr = self.grid
        phi = gr.phi

        phinew = gr.scratch_array()

        alpha = self.k*dt/gr.dx**2

        # create the RHS of the matrix
        R = phi[gr.ilo:gr.ihi+1]

        # create the diagonal, d+1 and d-1 parts of the matrix
        d = (1.0 + 2.0*alpha)*np.ones(gr.nx)
        u = -alpha*np.ones(gr.nx)
        u[0] = 0.0

        l = -alpha*np.ones(gr.nx)
        l[gr.nx-1] = 0.0

        # set the boundary conditions by changing the matrix elements

        # homogeneous neumann
        d[0] = 1.0 + alpha
        d[gr.nx-1] = 1.0 + alpha

        # solve
        A = np.matrix([u,d,l])
        phinew[gr.ilo:gr.ihi+1] = linalg.solve_banded((1,1), A, R)

        return phinew

    def evolve(self, C, tmax):
        """
        the main evolution loop.  Evolve
        
        phi_t = k phi_{xx}

        from t = 0 to tmax
        """

        gr = self.grid

        # time info
        dt = C*0.5*gr.dx**2/self.k

        while self.t < tmax:

            # make sure we end right at tmax
            if self.t + dt > tmax:
                dt = tmax - self.t

            # diffuse for dt
            phinew = self.diffuse_implicit(dt)

            gr.phi[:] = phinew[:]
            self.t += dt


if __name__ == "__main__":

    # reference time
    t0 = 1.e-4

    # state coeffs
    phi1 = 1.0
    phi2 = 2.0

    k = 1.0


    #-------------------------------------------------------------------------
    # normal time

    tmax = 0.005

    nx = 128

    Cs = [0.8, 2.0, 10.0]
    colors = ["b", "g", "r"]

    for C in Cs:
        gr = Grid1d(nx, ng=1)
        s = Simulation(gr, k=k)
        s.init_cond("gaussian", t0, phi1, phi2)
        s.evolve(C, tmax)

        plt.plot(gr.x[gr.ilo:gr.ihi+1], gr.phi[gr.ilo:gr.ihi+1], 
                 color=colors.pop(), label="C = {}".format(C))

    # analytic solution
    plt.plot(gr.x[gr.ilo:gr.ihi+1],
               gr.phi_a(tmax, k, t0, phi1, phi2)[gr.ilo:gr.ihi+1],
               ls=":", color="0.5", label="analytic solution")

    plt.legend(frameon=False)

    plt.xlabel("$x$", fontsize="large")
    plt.ylabel(r"$\phi$", fontsize="large")
    plt.title("Backward-difference implicit diffusion, nx = {}, C = {:3.2f}, t = {:5.2g}".format(nx, C, tmax))

    plt.savefig("diffimplicit.pdf")


    #-------------------------------------------------------------------------
    # early time

    plt.clf()

    tmax = 0.0005

    nx = 128

    colors = ["b", "g", "r"]

    for C in Cs:
        gr = Grid1d(nx, ng=1)
        s = Simulation(gr, k=k)
        s.init_cond("gaussian", t0, phi1, phi2)
        s.evolve(C, tmax)

        plt.plot(gr.x[gr.ilo:gr.ihi+1], gr.phi[gr.ilo:gr.ihi+1], 
                 color=colors.pop(), label="C = {}".format(C))

    # analytic solution
    plt.plot(gr.x[gr.ilo:gr.ihi+1],
               gr.phi_a(tmax, k, t0, phi1, phi2)[gr.ilo:gr.ihi+1],
               ls=":", color="0.5", label="analytic solution")

    plt.legend(frameon=False)

    plt.xlim(0.3,0.7)

    plt.xlabel("$x$", fontsize="large")
    plt.ylabel(r"$\phi$", fontsize="large")
    plt.title("Backward-difference implicit diffusion, nx = {}, C = {:3.2f}, t = {:5.2g}".format(nx, C, tmax))

    plt.savefig("diffimplicit-early.pdf")
