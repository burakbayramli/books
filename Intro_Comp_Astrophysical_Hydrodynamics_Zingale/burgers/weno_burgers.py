import numpy
from matplotlib import pyplot
import burgers
import weno_coefficients
from scipy.optimize import brentq

def burgers_sine_exact(x, t):
    """
    Compute the exact solution of Burger's given the 'sine' initial data
    """
#    def initial_sin(x):
#        if x < 1/3 or x > 2/3:
#            return 1
#        else:
#            return 1 + numpy.sin(6*numpy.pi*(x-1/3))/2
    def initial_smooth_sin(x):
        return numpy.sin(2*numpy.pi*x)
    def initial_gaussian(x):
        return 1.0 + numpy.exp(-60.0*(x - 0.5)**2)
    def residual(x_at_t_0_guess, x_at_t):
        q = initial_gaussian(x_at_t_0_guess)
        return x_at_t_0_guess + q * t - x_at_t
    
    q = numpy.zeros_like(x)
    for i in range(len(q)):
        x_at_t_0 = brentq(residual, -2, 2, args=(x[i],))
        q[i] = initial_gaussian(x_at_t_0)
    return q
        

def weno(order, q):
    """
    Do WENO reconstruction
    
    Parameters
    ----------
    
    order : int
        The stencil width
    q : numpy array
        Scalar data to reconstruct
        
    Returns
    -------
    
    qL : numpy array
        Reconstructed data - boundary points are zero
    """
    C = weno_coefficients.C_all[order]
    a = weno_coefficients.a_all[order]
    sigma = weno_coefficients.sigma_all[order]

    qL = numpy.zeros_like(q)
    beta = numpy.zeros((order, len(q)))
    w = numpy.zeros_like(beta)
    np = len(q) - 2 * order
    epsilon = 1e-16
    for i in range(order, np+order):
        q_stencils = numpy.zeros(order)
        alpha = numpy.zeros(order)
        for k in range(order):
            for l in range(order):
                for m in range(l+1):
                    beta[k, i] += sigma[k, l, m] * q[i+k-l] * q[i+k-m]
            alpha[k] = C[k] / (epsilon + beta[k, i]**2)
            for l in range(order):
                q_stencils[k] += a[k, l] * q[i+k-l]
        w[:, i] = alpha / numpy.sum(alpha)
        qL[i] = numpy.dot(w[:, i], q_stencils)
    
    return qL


def weno_M(order, q):
    """
    Do WENOM reconstruction following Gerolymos equation (18)
    
    Parameters
    ----------
    
    order : int
        The stencil width
    q : numpy array
        Scalar data to reconstruct
        
    Returns
    -------
    
    qL : numpy array
        Reconstructed data - boundary points are zero
    """
    C = weno_coefficients.C_all[order]
    a = weno_coefficients.a_all[order]
    sigma = weno_coefficients.sigma_all[order]

    qL = numpy.zeros_like(q)
    beta = numpy.zeros((order, len(q)))
    w = numpy.zeros_like(beta)
    np = len(q) - 2 * order
    epsilon = 1e-16
    for i in range(order, np+order):
        q_stencils = numpy.zeros(order)
        alpha_JS = numpy.zeros(order)
        for k in range(order):
            for l in range(order):
                for m in range(l+1):
                    beta[k, i] += sigma[k, l, m] * q[i+k-l] * q[i+k-m]
            alpha_JS[k] = C[k] / (epsilon + beta[k, i]**2)
            for l in range(order):
                q_stencils[k] += a[k, l] * q[i+k-l]
        w_JS = alpha_JS / numpy.sum(alpha_JS)
        alpha = w_JS * (C + C**2 - 3 * C * w_JS + w_JS**2) / \
                       (C**2 + w_JS * (1 - 2 * C))
        w[:, i] = alpha / numpy.sum(alpha)
        qL[i] = numpy.dot(w[:, i], q_stencils)
    
    return qL


class WENOSimulation(burgers.Simulation):
    
    def __init__(self, grid, C=0.5, weno_order=3):
        self.grid = grid
        self.t = 0.0 # simulation time
        self.C = C   # CFL number
        self.weno_order = weno_order

    def init_cond(self, type="tophat"):
        if type == "smooth_sine":
            self.grid.u = numpy.sin(2 * numpy.pi * self.grid.x)
        elif type == "gaussian":
            self.grid.u = 1.0 + numpy.exp(-60.0*(self.grid.x - 0.5)**2)
        else:
            super().init_cond(type)


    def burgers_flux(self, q):
        return 0.5*q**2


    def rk_substep(self):
        
        g = self.grid
        g.fill_BCs()
        f = self.burgers_flux(g.u)
        alpha = numpy.max(abs(g.u))
        fp = (f + alpha * g.u) / 2
        fm = (f - alpha * g.u) / 2
        fpr = g.scratch_array()
        fml = g.scratch_array()
        flux = g.scratch_array()
        fpr[1:] = weno(self.weno_order, fp[:-1])
        fml[-1::-1] = weno(self.weno_order, fm[-1::-1])
        flux[1:-1] = fpr[1:-1] + fml[1:-1]
        rhs = g.scratch_array()
        rhs[1:-1] = 1/g.dx * (flux[1:-1] - flux[2:])
        return rhs


    def evolve(self, tmax):
        """ evolve the linear advection equation using RK4 """
        self.t = 0.0
        g = self.grid

        # main evolution loop
        while self.t < tmax:

            # fill the boundary conditions
            g.fill_BCs()

            # get the timestep
            dt = self.timestep(self.C)

            if self.t + dt > tmax:
                dt = tmax - self.t

            # RK4
            # Store the data at the start of the step
            u_start = g.u.copy()
            k1 = dt * self.rk_substep()
            g.u = u_start + k1 / 2
            k2 = dt * self.rk_substep()
            g.u = u_start + k2 / 2
            k3 = dt * self.rk_substep()
            g.u = u_start + k3
            k4 = dt * self.rk_substep()
            g.u = u_start + (k1 + 2 * (k2 + k3) + k4) / 6

            self.t += dt



if __name__ == "__main__":

    #-----------------------------------------------------------------------------
    # sine
    
    xmin = 0.0
    xmax = 1.0
    nx = 256
    order = 3
    ng = order+1
    g = burgers.Grid1d(nx, ng, bc="periodic")
    
    # maximum evolution time based on period for unit velocity
    tmax = (xmax - xmin)/1.0
    
    C = 0.5
    
    pyplot.clf()
    
    s = WENOSimulation(g, C, order)
    
    for i in range(0, 10):
        tend = (i+1)*0.02*tmax
        s.init_cond("sine")
    
        uinit = s.grid.u.copy()
    
        s.evolve(tend)
    
        c = 1.0 - (0.1 + i*0.1)
        g = s.grid
        pyplot.plot(g.x[g.ilo:g.ihi+1], g.u[g.ilo:g.ihi+1], color=str(c))
    
    
    g = s.grid
    pyplot.plot(g.x[g.ilo:g.ihi+1], uinit[g.ilo:g.ihi+1], ls=":", color="0.9", zorder=-1)
    
    pyplot.xlabel("$x$")
    pyplot.ylabel("$u$")
    pyplot.savefig("weno-burger-sine.pdf")
    
    # Compare the WENO and "standard" (from burgers.py) results at low res
    nx = 64
    tend = 0.2
    g_hires = burgers.Grid1d(512, ng, bc="periodic")
    s_hires = WENOSimulation(g_hires, C, order)
    s_hires.init_cond("sine")
    s_hires.evolve(tend)
    gW3 = burgers.Grid1d(nx, 4, bc="periodic")
    sW3 = WENOSimulation(gW3, C, 3)
    sW3.init_cond("sine")
    sW3.evolve(tend)
    gW5 = burgers.Grid1d(nx, 6, bc="periodic")
    sW5 = WENOSimulation(gW5, C, 5)
    sW5.init_cond("sine")
    sW5.evolve(tend)
    g = burgers.Grid1d(nx, ng, bc="periodic")
    s = burgers.Simulation(g)
    s.init_cond("sine")
    s.evolve(C, tend)
    pyplot.clf()
    pyplot.plot(g_hires.x[g_hires.ilo:g_hires.ihi+1], 
                g_hires.u[g_hires.ilo:g_hires.ihi+1], 'k--', label='High resolution')
    pyplot.plot(g.x[g.ilo:g.ihi+1], g.u[g.ilo:g.ihi+1], 'gd', label='PLM, MC')
    pyplot.plot(gW3.x[gW3.ilo:gW3.ihi+1], gW3.u[gW3.ilo:gW3.ihi+1], 'bo', label='WENO, r=3')
    pyplot.plot(gW5.x[gW5.ilo:gW5.ihi+1], gW5.u[gW5.ilo:gW5.ihi+1], 'r^', label='WENO, r=5')
    pyplot.xlabel("$x$")
    pyplot.ylabel("$u$")
    pyplot.legend()
    pyplot.xlim(0.5, 0.9)
    pyplot.legend(frameon=False)
    pyplot.savefig("weno-vs-plm-burger.pdf")
    
    
    #-----------------------------------------------------------------------------
    # rarefaction
    
    xmin = 0.0
    xmax = 1.0
    nx = 256
    order = 3
    ng = order+1
    g = burgers.Grid1d(nx, ng, bc="outflow")
    
    # maximum evolution time based on period for unit velocity
    tmax = (xmax - xmin)/1.0
    
    C = 0.5
    
    pyplot.clf()
    
    s = WENOSimulation(g, C, order)
    
    for i in range(0, 10):
        tend = (i+1)*0.02*tmax
    
        s.init_cond("rarefaction")
    
        uinit = s.grid.u.copy()
    
        s.evolve(tend)
    
        c = 1.0 - (0.1 + i*0.1)
        pyplot.plot(g.x[g.ilo:g.ihi+1], g.u[g.ilo:g.ihi+1], color=str(c))
    
    
    pyplot.plot(g.x[g.ilo:g.ihi+1], uinit[g.ilo:g.ihi+1], ls=":", color="0.9", zorder=-1)
    
    pyplot.xlabel("$x$")
    pyplot.ylabel("$u$")
    
    pyplot.savefig("weno-burger-rarefaction.pdf")

    #-----------------------------------------------------------------------
    # Convergence test at t = 0.1 using gaussian data
    
    
    problem = "gaussian"

    xmin = 0.0
    xmax = 1.0
    tmax = 0.05
    orders = [3, 4]
    N = [64, 81, 108, 128, 144, 192, 256]
    #N = 2**numpy.arange(5,10)
    C = 0.5

    errs = []

    colors="brcg"

    for order in orders:
        ng = order+1
        errs.append([])
        for nx in N:
            print(order, nx)
            gu = burgers.Grid1d(nx, ng, xmin=xmin, xmax=xmax)
            su = WENOSimulation(gu, C=0.5, weno_order=order)
        
            su.init_cond("gaussian")
        
            su.evolve(tmax)
            
            uexact = burgers_sine_exact(gu.x, tmax)
        
            errs[-1].append(gu.norm(gu.u - uexact))
    
    pyplot.clf()
    N = numpy.array(N, dtype=numpy.float64)
    for n_order, order in enumerate(orders):
        pyplot.scatter(N, errs[n_order],
                       color=colors[n_order],
                       label=r"WENO, $r={}$".format(order))
    pyplot.plot(N, errs[0][-2]*(N[-2]/N)**(5),
                linestyle="--", color=colors[0],
                label=r"$\mathcal{{O}}(\Delta x^{{{}}})$".format(5))
    pyplot.plot(N, errs[1][-3]*(N[-3]/N)**(7),
                linestyle="--", color=colors[1],
                label=r"$\mathcal{{O}}(\Delta x^{{{}}})$".format(7))

    ax = pyplot.gca()
    ax.set_ylim(numpy.min(errs)/5, numpy.max(errs)*5)
    ax.set_xscale('log')
    ax.set_yscale('log')

    pyplot.xlabel("N")
    pyplot.ylabel(r"$\| a^\mathrm{final} - a^\mathrm{init} \|_2$",
               fontsize=16)
    pyplot.title("Convergence of Burger's, Gaussian, RK4")

    pyplot.legend(frameon=False)
    pyplot.savefig("weno-converge-burgers.pdf")