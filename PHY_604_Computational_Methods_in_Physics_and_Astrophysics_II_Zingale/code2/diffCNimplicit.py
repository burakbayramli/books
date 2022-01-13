"""
solve the diffusion equation:

 phi_t = k phi_{xx} 

with a Crank-Nicolson implicit discretization

M. Zingale (2013-04-03)
"""

import numpy as np
from scipy import linalg
import matplotlib.pylab as plt
import diffimplicit

def diffuse_CN(gr, k, dt):
    """ diffuse phi implicitly through timestep dt, with a C-N
        temporal discretization """

    phinew = gr.scratch_array()
    
    alpha = k*dt/gr.dx**2

    # create the RHS of the matrix
    gr.fill_BCs()
    R = 0.5*k*dt*lap(gr, gr.phi)
    R = R[gr.ilo:gr.ihi+1]
    R += gr.phi[gr.ilo:gr.ihi+1] 
    
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


def lap(gr, phi):
    """ compute the Laplacian of phi """

    lapphi = gr.scratch_array()

    ib = gr.ilo
    ie = gr.ihi

    lapphi[ib:ie+1] = (phi[ib-1:ie] - 2.0*phi[ib:ie+1] + phi[ib+1:ie+2])/gr.dx**2

    return lapphi


class Grid(object):

    def __init__(self, nx, ng=1, xmin=0.0, xmax=1.0):
        """ grid class initialization """
        
        self.nx = nx
        self.ng = ng

        self.xmin = xmin
        self.xmax = xmax

        self.dx = (xmax - xmin)/nx
        self.x = (np.arange(nx+2*ng) + 0.5 - ng)*self.dx + xmin

        self.ilo = ng
        self.ihi = ng+nx-1

        # storage for the solution
        self.phi = np.zeros((nx+2*ng), dtype=np.float64)

    def fill_BCs(self):
        """ fill the Neumann BCs """

        # Neumann BCs
        self.phi[0:self.ilo]  = self.phi[self.ilo]
        self.phi[self.ihi+1:] = self.phi[self.ihi]

    def scratch_array(self):
        return np.zeros((2*self.ng+self.nx), dtype=np.float64)

    def phi_a(self, t, k, t0, phi1, phi2):
        """ analytic solution """

        xc = 0.5*(self.xmin + self.xmax)
        return (phi2 - phi1)*np.sqrt(t0/(t + t0)) * \
            np.exp(-0.25*(self.x-xc)**2/(k*(t + t0))) + phi1

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))


def evolve(nx, k, t0, phi1, phi2, C, tmax):
    """ 
    the main evolution loop.  Evolve 
  
     phi_t = k phi_{xx} 

    from t = 0 to tmax
    """

    # create the grid
    gr = Grid(nx, ng=1, xmax=1.0)

    # time info
    dt = C*0.5*gr.dx**2/k
    t = 0.0

    # initialize the data
    gr.phi[:] = gr.phi_a(0.0, k, t0, phi1, phi2)

    while t < tmax:

        gr.fill_BCs()

        # make sure we end right at tmax
        if t + dt > tmax:
            dt = tmax - t

        # diffuse for dt
        phinew = diffuse_CN(gr, k, dt)

        gr.phi[:] = phinew[:]
        t += dt

    return gr


def evolve_explicit(nx, k, t0, phi1, phi2, C, tmax):
    """ fully explicit for comparison """

    ng = 1

    # create the grid
    g = Grid(nx, ng, xmax=1.0)

    # time info
    dt = C*0.5*g.dx**2/k
    t = 0.0

    # initialize the data
    g.phi[:] = g.phi_a(0.0, k, t0, phi1, phi2)

    # evolution loop
    phinew = g.scratch_array()
    
    while t < tmax:

        # make sure we end right at tmax
        if t + dt > tmax:
            dt = tmax - t

        # fill the boundary conditions
        g.fill_BCs()

        alpha = k*dt/g.dx**2

        # loop over zones
        for i in range(g.ilo, g.ihi+1):
            phinew[i] = g.phi[i] + alpha*(g.phi[i+1] - 2.0*g.phi[i] + g.phi[i-1])

        # store the updated solution
        g.phi[:] = phinew[:]
        t += dt

    return g

#-----------------------------------------------------------------------------
# convergence C = 0.8

plt.clf()

# a characteristic timescale for diffusion if L^2/k
tmax = 0.005

t0 = 1.e-4
phi1 = 1.0
phi2 = 2.0

k = 1.0

N = [32, 64, 128, 256, 512]
ng = 1

# CFL number
C = 0.8

err = []
errFOimpl = []
errExpl = []

for nx in N:

    # the present C-N discretization
    g = evolve(nx, k, t0, phi1, phi2, C, tmax)

    # compare to the first-order implicit discretization
    gFOimpl = diffimplicit.evolve(nx, k, t0, phi1, phi2, C, tmax)

    # compare to the explicit discretization
    gExpl = evolve_explicit(nx, k, t0, phi1, phi2, C, tmax)
    
    phi_analytic = g.phi_a(tmax, k, t0, phi1, phi2)

    err.append(g.norm(g.phi - phi_analytic))
    errFOimpl.append(g.norm(gFOimpl.phi - phi_analytic))
    errExpl.append(g.norm(gExpl.phi - phi_analytic))


plt.clf()

N = np.array(N, dtype=np.float64)
err = np.array(err)

print "err = ", err
print "errFOimpl = ", errFOimpl
print "errExpl = ", errExpl

plt.scatter(N, err, color="r", label="C-N implicit diffusion")
plt.scatter(N, errFOimpl, color="b", label="backward-diff implicit diffusion")
plt.scatter(N, errExpl, color="g", label="forward-diff explicit diffusion")
plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="k", label="$\mathcal{O}(\Delta x^2)$")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel(r"$N$")
plt.ylabel(r"L2 norm of absolute error")
plt.title("Convergence of Diffusion Methods, C = %3.2f, t = %5.2g" % (C, tmax))

plt.ylim(1.e-6, 1.e-2)
plt.legend(frameon=False, fontsize="small")

plt.savefig("diffmethods-converge-0.8.png")



#-----------------------------------------------------------------------------
# convergence C = 2.0

plt.clf()

# a characteristic timescale for diffusion if L^2/k
tmax = 0.005

t0 = 1.e-4
phi1 = 1.0
phi2 = 2.0

k = 1.0

N = [32, 64, 128, 256, 512]
ng = 1

# CFL number
C = 2.0

err = []
errFOimpl = []
errExpl = []

for nx in N:

    # the present C-N discretization
    g = evolve(nx, k, t0, phi1, phi2, C, tmax)

    # compare to the first-order implicit discretization
    gFOimpl = diffimplicit.evolve(nx, k, t0, phi1, phi2, C, tmax)

    phi_analytic = g.phi_a(tmax, k, t0, phi1, phi2)

    err.append(g.norm(g.phi - phi_analytic))
    errFOimpl.append(g.norm(gFOimpl.phi - phi_analytic))


plt.clf()

N = np.array(N, dtype=np.float64)
err = np.array(err)

print "err = ", err
print "errFOimpl = ", errFOimpl

plt.scatter(N, err, color="r", label="C-N implicit diffusion")
plt.scatter(N, errFOimpl, color="b", label="backward-diff implicit diffusion")
plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="k", label="$\mathcal{O}(\Delta x^2)$")

ax = plt.gca()
ax.set_xscale('log')
ax.set_yscale('log')

plt.xlabel(r"$N$")
plt.ylabel(r"L2 norm of absolute error")
plt.title("Convergence of Diffusion Methods, C = %3.2f, t = %5.2g" % (C, tmax))

plt.ylim(1.e-6, 1.e-2)
plt.legend(frameon=False, fontsize="small")

plt.savefig("diffmethods-converge-2.0.png")


