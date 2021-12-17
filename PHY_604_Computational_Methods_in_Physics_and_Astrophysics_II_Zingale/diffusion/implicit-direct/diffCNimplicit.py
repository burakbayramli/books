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
    A = np.matrix([u, d, l])
    phinew[gr.ilo:gr.ihi+1] = linalg.solve_banded((1, 1), A, R)

    return phinew


def lap(gr, phi):
    """ compute the Laplacian of phi """

    lapphi = gr.scratch_array()

    ib = gr.ilo
    ie = gr.ihi

    lapphi[ib:ie+1] = (phi[ib-1:ie] - 2.0*phi[ib:ie+1] + phi[ib+1:ie+2])/gr.dx**2

    return lapphi


def evolve(nx, k, t0, phi1, phi2, C, tmax):
    """
    the main evolution loop.  Evolve

     phi_t = k phi_{xx}

    from t = 0 to tmax
    """

    # create the grid
    gr = diffimplicit.Grid(nx, ng=1, xmax=1.0)

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
    g = diffimplicit.Grid(nx, ng, xmax=1.0)

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


def convergence_plot(C):

    plt.clf()

    # a characteristic timescale for diffusion if L^2/k
    tmax = 0.005

    t0 = 1.e-4
    phi1 = 1.0
    phi2 = 2.0

    k = 1.0

    N = [32, 64, 128, 256, 512]
    ng = 1

    err = []
    err_fo_impl = []
    err_expl = []

    for nx in N:

        # the present C-N discretization
        g = evolve(nx, k, t0, phi1, phi2, C, tmax)

        # compare to the first-order implicit discretization
        gFOimpl = diffimplicit.evolve(nx, k, t0, phi1, phi2, C, tmax)

        if C < 1:
            # compare to the explicit discretization
            gExpl = evolve_explicit(nx, k, t0, phi1, phi2, C, tmax)

        phi_analytic = g.phi_a(tmax, k, t0, phi1, phi2)

        err.append(g.norm(g.phi - phi_analytic))
        err_fo_impl.append(g.norm(gFOimpl.phi - phi_analytic))
        if C < 1:
            err_expl.append(g.norm(gExpl.phi - phi_analytic))

    N = np.array(N, dtype=np.float64)
    err = np.array(err)

    print("err = ", err)
    print("err_fo_impl = ", err_fo_impl)
    print("err_expl = ", err_expl)

    plt.scatter(N, err, label="C-N implicit diffusion")
    plt.scatter(N, err_fo_impl, label="backward-diff implicit diffusion")
    if C < 1:
        plt.scatter(N, err_expl, label="forward-diff explicit diffusion")
    plt.plot(N, err[len(N)-1]*(N[len(N)-1]/N)**2, color="0.5", label=r"$\mathcal{O}(\Delta x^2)$")

    ax = plt.gca()
    ax.set_xscale('log')
    ax.set_yscale('log')

    plt.xlabel(r"$N$")
    plt.ylabel(r"L2 norm of absolute error")
    plt.title("Convergence of Diffusion Methods, C = {:3.2f}, t = {:5.2g}".format(C, tmax))

    plt.ylim(1.e-6, 1.e-2)
    plt.legend(frameon=False, fontsize="small")

    plt.savefig("diffmethods-converge-{}.png".format(C), dpi=150)


def unstable():
    """ show an example of Crank-Nicolson doing bad things """

    plt.clf()

    # a characteristic timescale for diffusion if L^2/k
    tmax = 0.0008
    C = 10.0

    t0 = 1.e-4
    phi1 = 1.0
    phi2 = 2.0

    k = 1.0

    N = 64
    ng = 1

    # the present C-N discretization
    g = evolve(N, k, t0, phi1, phi2, C, tmax)
    phi_analytic = g.phi_a(tmax, k, t0, phi1, phi2)

    plt.plot(g.x[g.ilo:g.ihi+1], g.phi[g.ilo:g.ihi+1])
    plt.plot(g.x[g.ilo:g.ihi+1], phi_analytic[g.ilo:g.ihi+1], ls=":")

    plt.ylabel(r"$\phi$")
    plt.xlabel(r"$x$")

    plt.title("C-N implicit diffusion, N = 64, C = 10")

    plt.savefig("CN_unstable.png", dpi=150)


if __name__ == "__main__":
    convergence_plot(0.8)
    convergence_plot(2.0)
    unstable()

