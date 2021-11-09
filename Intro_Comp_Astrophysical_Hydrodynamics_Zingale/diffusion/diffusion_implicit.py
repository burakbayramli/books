"""
solve the diffusion equation:

 phi_t = k phi_{xx}

with a Crank-Nicolson implicit discretization

M. Zingale (2013-04-03)
"""

import numpy as np
from scipy import linalg
import matplotlib.pyplot as plt
from diffusion_explicit import Grid1d

import matplotlib as mpl

# Use LaTeX for rendering
mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'
mpl.rcParams['font.size'] = 12
mpl.rcParams['legend.fontsize'] = 'large'
mpl.rcParams['figure.titlesize'] = 'small'

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

    def diffuse_CN(self, dt):
        """
        diffuse phi implicitly through timestep dt, with a C-N
        temporal discretization
        """

        gr = self.grid
        phi = gr.phi

        phinew = gr.scratch_array()

        alpha = self.k*dt/gr.dx**2

        # create the RHS of the matrix
        gr.fill_BCs()
        R = 0.5*self.k*dt*self.lap()
        R = R[gr.ilo:gr.ihi+1]
        R += phi[gr.ilo:gr.ihi+1]

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

        # Dirichlet
        #d[0] = 1.0 + 1.5*alpha
        #d[gr.nx-1] = 1.0 + 1.5*alpha

        #R[0] += alpha*phi1
        #R[gr.nx-1] += alpha*phi1

        # solve
        A = np.matrix([u,d,l])
        phinew[gr.ilo:gr.ihi+1] = linalg.solve_banded((1,1), A, R)

        return phinew

    def lap(self):
        """ compute the Laplacian of phi """

        gr = self.grid
        phi = gr.phi

        lapphi = gr.scratch_array()

        ib = gr.ilo
        ie = gr.ihi

        lapphi[ib:ie+1] = \
            (phi[ib-1:ie] - 2.0*phi[ib:ie+1] + phi[ib+1:ie+2])/gr.dx**2

        return lapphi

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

            gr.fill_BCs()

            # make sure we end right at tmax
            if self.t + dt > tmax:
                dt = tmax - self.t

            # diffuse for dt
            phinew = self.diffuse_CN(dt)

            gr.phi[:] = phinew[:]
            self.t += dt


if __name__ == "__main__":

    #--------------------------------------------------------------------------
    # Convergence of a Gaussian

    # a characteristic timescale for diffusion is L^2/k
    tmax = 0.005

    t0 = 1.e-4
    phi1 = 1.0
    phi2 = 2.0

    k = 1.0

    N = np.array([32, 64, 128, 256, 512])

    # CFL number
    CFL = [0.8, 8.0]

    for C in CFL:
        err = []

        for nx in N:

            # the present C-N discretization
            print(C, nx)
            g = Grid1d(nx, ng=1)
            s = Simulation(g, k=k)
            s.init_cond("gaussian", t0, phi1, phi2)
            s.evolve(C, tmax)
        
            xc = 0.5*(g.xmin + g.xmax)
            phi_analytic = g.phi_a(tmax, k, t0, phi1, phi2)
            
            err.append(g.norm(g.phi - phi_analytic))

        plt.clf()

        err = np.array(err)

        plt.scatter(N, err, color="C0", label="C-N implicit diffusion")
        plt.loglog(N, err[len(N)-1]*(N[len(N)-1]/N)**2, 
                   color="C1", label="$\mathcal{O}(\Delta x^2)$")

        plt.xlabel(r"$N$", fontsize="large")
        plt.ylabel(r"L2 norm of absolute error")
        plt.title("C-N Implicit Diffusion, C = %3.2f, t = %5.2g" % (C, tmax))

        plt.ylim(1.e-6, 1.e-2)
        plt.legend(frameon=False, fontsize="small")

        plt.tight_layout()
        
        plt.savefig("diffimplicit-converge-{}.pdf".format(C))


    #-------------------------------------------------------------------------
    # solution at multiple times

    # diffusion coefficient
    k = 1.0

    # reference time
    t0 = 1.e-4

    # state coeffs
    phi1 = 1.0
    phi2 = 2.0

    nx = 128

    # a characteristic timescale for diffusion is 0.5*dx**2/k
    dt = 0.5/(k*nx**2)
    tmax = 100*dt

    # analytic on a fine grid
    nx_analytic = 512

    CFL = [0.8, 8.0]

    for C in CFL:

        plt.clf()

        ntimes = 5
        tend = tmax/2.0**(ntimes-1)

        c = ["C0", "C1", "C2", "C3", "C4"]

        while tend <= tmax:

            g = Grid1d(nx, ng=2)
            s = Simulation(g, k=k)
            s.init_cond("gaussian", t0, phi1, phi2)
            s.evolve(C, tend)

            ga = Grid1d(nx_analytic, ng=2)
            xc = 0.5*(ga.xmin + ga.xmax)
            phi_analytic = ga.phi_a(tend, k, t0, phi1, phi2)

            color = c.pop()
            plt.plot(g.x[g.ilo:g.ihi+1], g.phi[g.ilo:g.ihi+1],
                     "x", color=color, label="$t = %g$ s" % (tend))
            plt.plot(ga.x[ga.ilo:ga.ihi+1], phi_analytic[ga.ilo:ga.ihi+1],
                     color=color, ls="-")

            tend = 2.0*tend


        plt.xlim(0.35,0.65)
        plt.ylim(0.95,1.7)

        plt.legend(frameon=False, fontsize="small")

        plt.xlabel("$x$", fontsize="large")
        plt.ylabel(r"$\phi$", fontsize="large")
        plt.title(r"implicit diffusion, N = %d, $C$ = %3.2f" % (nx, C))

        f = plt.gcf()
        f.set_size_inches(8.0, 6.0)

        plt.tight_layout()
        plt.savefig("diff-implicit-{}-CFL_{}.pdf".format(nx, C))


    # under-resolved example
    plt.clf()
    nx = 64
    C = 10.0
    tmax = 0.001

    g = Grid1d(nx, ng=2)
    s = Simulation(g, k=k)
    s.init_cond("gaussian", t0, phi1, phi2)
    s.evolve(C, tend)

    ga = Grid1d(nx_analytic, ng=2)
    xc = 0.5*(ga.xmin + ga.xmax)
    phi_analytic = ga.phi_a(tend, k, t0, phi1, phi2)

    plt.plot(g.x[g.ilo:g.ihi+1], g.phi[g.ilo:g.ihi+1],
             color="r", marker="x", ls="-", label="$t = %g$ s" % (tend))
    plt.plot(ga.x[ga.ilo:ga.ihi+1], phi_analytic[ga.ilo:ga.ihi+1],
             color=color, ls="-")

    plt.xlim(0.2,0.8)

    plt.xlabel("$x$", fontsize="large")
    plt.ylabel(r"$\phi$", fontsize="large")
    plt.title(r"implicit diffusion, N = {}, $C$ = {:3.2f}, $t$ = {}".format(nx, C, tmax))

    f = plt.gcf()
    f.set_size_inches(8.0, 6.0)

    plt.tight_layout()

    plt.savefig("diff-implicit-{}-CFL_{}.pdf".format(nx, C))
