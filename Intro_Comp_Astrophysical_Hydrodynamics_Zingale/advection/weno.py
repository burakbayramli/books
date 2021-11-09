import numpy
from matplotlib import pyplot
import advection
import weno_coefficients
from scipy.integrate import ode


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


class WENOSimulation(advection.Simulation):
    
    def __init__(self, grid, u, C=0.8, weno_order=3):
        self.grid = grid
        self.t = 0.0 # simulation time
        self.u = u   # the constant advective velocity
        self.C = C   # CFL number
        self.weno_order = weno_order


    def init_cond(self, type="tophat"):
        """ initialize the data """
        if type == "sine_sine":
            self.grid.a[:] = numpy.sin(numpy.pi*self.grid.x - 
                       numpy.sin(numpy.pi*self.grid.x) / numpy.pi)
        else:
            super().init_cond(type)


    def rk_substep(self):
        
        g = self.grid
        g.fill_BCs()
        f = self.u * g.a
        alpha = abs(self.u)
        fp = (f + alpha * g.a) / 2
        fm = (f - alpha * g.a) / 2
        fpr = g.scratch_array()
        fml = g.scratch_array()
        flux = g.scratch_array()
        fpr[1:] = weno(self.weno_order, fp[:-1])
        fml[-1::-1] = weno(self.weno_order, fm[-1::-1])
        flux[1:-1] = fpr[1:-1] + fml[1:-1]
        rhs = g.scratch_array()
        rhs[1:-1] = 1/g.dx * (flux[1:-1] - flux[2:])
        return rhs


    def evolve(self, num_periods=1):
        """ evolve the linear advection equation using RK4 """
        self.t = 0.0
        g = self.grid

        tmax = num_periods*self.period()

        # main evolution loop
        while self.t < tmax:

            # fill the boundary conditions
            g.fill_BCs()

            # get the timestep
            dt = self.timestep()

            if self.t + dt > tmax:
                dt = tmax - self.t

            # RK4
            # Store the data at the start of the step
            a_start = g.a.copy()
            k1 = dt * self.rk_substep()
            g.a = a_start + k1 / 2
            k2 = dt * self.rk_substep()
            g.a = a_start + k2 / 2
            k3 = dt * self.rk_substep()
            g.a = a_start + k3
            k4 = dt * self.rk_substep()
            g.a = a_start + (k1 + 2 * (k2 + k3) + k4) / 6

            self.t += dt


    def evolve_scipy(self, num_periods=1):
        """ evolve the linear advection equation using RK4 """
        self.t = 0.0
        g = self.grid
        
        def rk_substep_scipy(t, y):
            # Periodic BCs
            y[:g.ng] = y[-2*g.ng:-g.ng]
            y[-g.ng:] = y[g.ng:2*g.ng]
            f = self.u * y
            alpha = abs(self.u)
            fp = (f + alpha * y) / 2
            fm = (f - alpha * y) / 2
            fpr = g.scratch_array()
            fml = g.scratch_array()
            flux = g.scratch_array()
            fpr[1:] = weno(self.weno_order, fp[:-1])
            fml[-1::-1] = weno(self.weno_order, fm[-1::-1])
            flux[1:-1] = fpr[1:-1] + fml[1:-1]
            rhs = g.scratch_array()
            rhs[1:-1] = 1/g.dx * (flux[1:-1] - flux[2:])
            return rhs

        tmax = num_periods*self.period()
        r = ode(rk_substep_scipy).set_integrator('dop853')
        r.set_initial_value(g.a, 0)
        dt = self.timestep()

        # main evolution loop
        while r.successful() and r.t < tmax:
            dt = min(dt, tmax - r.t)
            r.integrate(r.t+dt)
        g.a[:] = r.y


class WENOMSimulation(WENOSimulation):

    def rk_substep(self):
        
        g = self.grid
        g.fill_BCs()
        f = self.u * g.a
        alpha = abs(self.u)
        fp = (f + alpha * g.a) / 2
        fm = (f - alpha * g.a) / 2
        fpr = g.scratch_array()
        fml = g.scratch_array()
        flux = g.scratch_array()
        fpr[1:] = weno_M(self.weno_order, fp[:-1])
        fml[-1::-1] = weno_M(self.weno_order, fm[-1::-1])
        flux[1:-1] = fpr[1:-1] + fml[1:-1]
        rhs = g.scratch_array()
        rhs[1:-1] = 1/g.dx * (flux[1:-1] - flux[2:])
        return rhs
    
    
    def evolve_scipy(self, num_periods=1):
        """ evolve the linear advection equation using scipy """
        self.t = 0.0
        g = self.grid
        
        def rk_substep_scipy(t, y):
            # Periodic BCs
            y[:g.ng] = y[-2*g.ng:-g.ng]
            y[-g.ng:] = y[g.ng:2*g.ng]
            f = self.u * y
            alpha = abs(self.u)
            fp = (f + alpha * y) / 2
            fm = (f - alpha * y) / 2
            fpr = g.scratch_array()
            fml = g.scratch_array()
            flux = g.scratch_array()
            fpr[1:] = weno_M(self.weno_order, fp[:-1])
            fml[-1::-1] = weno_M(self.weno_order, fm[-1::-1])
            flux[1:-1] = fpr[1:-1] + fml[1:-1]
            rhs = g.scratch_array()
            rhs[1:-1] = 1/g.dx * (flux[1:-1] - flux[2:])
            return rhs

        tmax = num_periods*self.period()
        r = ode(rk_substep_scipy).set_integrator('dop853')
        r.set_initial_value(g.a, 0)
        dt = self.timestep()

        # main evolution loop
        while r.successful() and r.t < tmax:
            dt = min(dt, tmax - r.t)
            r.integrate(r.t+dt)
        g.a[:] = r.y
    

if __name__ == "__main__":


    #-------------------------------------------------------------------------
    # compute WENO3 case

    xmin = 0.0
    xmax = 1.0
    nx = 64
    order = 3
    ng = order+1

    g = advection.Grid1d(nx, ng, xmin=xmin, xmax=xmax)

    u = 1.0
    
    s = WENOSimulation(g, u, C=0.5, weno_order=3)

    s.init_cond("gaussian")
    ainit = s.grid.a.copy()

    s.evolve(num_periods=1)

    pyplot.plot(g.x[g.ilo:g.ihi+1], ainit[g.ilo:g.ihi+1],
             ls=":", label="exact")

    pyplot.plot(g.x[g.ilo:g.ihi+1], g.a[g.ilo:g.ihi+1],
             label="WENO3")
    
    
#    #-------------------------------------------------------------------------
#    # convergence test
#    # Note that WENO schemes with standard weights lose convergence at
#    # critical points. For high degree critical points they lose more orders.
#    # The suggestion in Gerolymos is that you may expect to drop down to
#    # order r-1 in the limit.
#    # The Gaussian has all odd derivatives vanishing at the origin, so
#    # the higher order schemes will lose accuracy.
#    # For the Gaussian:
#    # This shows clean 5th order convergence for r=3
#    # But for r=4-6 the best you get is ~6th order, and 5th order is more
#    # realistic
#    # For sin(x - sin(x)) type data Gerolymos expects better results
#    # But the problem actually appears to be the time integrator
#    # Switching to Dormand-Price 8th order from scipy (a hack) will make it
#    # work for all cases. With sin(.. sin) data you get 2r - 2 thanks to
#    # the one critical point.
#    
#    problem = "sine_sine"
#
#    xmin =-1.0
#    xmax = 1.0
##    orders = [4]
#    orders = [3, 4, 5, 6]
##    N1 = [2**4*3**i//2**i for i in range(5)]
##    N2 = [2**5*3**i//2**i for i in range(6)]
##    N3 = [3**4*4**i//3**i for i in range(5)]
##    N4 = [2**(4+i) for i in range(4)]
##    N = numpy.unique(numpy.array(N1+N2+N3+N4, dtype=numpy.int))
##    N.sort()
##    N = [32, 64, 128, 256, 512]
##    N = [32, 64, 128]
#    N = [24, 32, 54, 64, 81, 108, 128]
#
#    errs = []
#    errsM = []
#
#    u = 1.0
#
#    colors="bygrc"
#
#    for order in orders:
#        ng = order+1
#        errs.append([])
#        errsM.append([])
#        for nx in N:
#            print(order, nx)
#            gu = advection.Grid1d(nx, ng, xmin=xmin, xmax=xmax)
#            su = WENOSimulation(gu, u, C=0.5, weno_order=order)
##            guM = advection.Grid1d(nx, ng, xmin=xmin, xmax=xmax)
##            suM = WENOMSimulation(guM, u, C=0.5, weno_order=order)
#        
#            su.init_cond("sine_sine")
##            suM.init_cond("sine_sine")
#            ainit = su.grid.a.copy()
#        
#            su.evolve_scipy(num_periods=1)
##            suM.evolve_scipy(num_periods=1)
#        
#            errs[-1].append(gu.norm(gu.a - ainit))
##            errsM[-1].append(guM.norm(guM.a - ainit))
#    
#    pyplot.clf()
#    N = numpy.array(N, dtype=numpy.float64)
#    for n_order, order in enumerate(orders):
#        pyplot.scatter(N, errs[n_order],
#                       color=colors[n_order],
#                       label=r"WENO, $r={}$".format(order))
##        pyplot.scatter(N, errsM[n_order],
##                       color=colors[n_order],
##                       label=r"WENOM, $r={}$".format(order))
#        pyplot.plot(N, errs[n_order][0]*(N[0]/N)**(2*order-2),
#                    linestyle="--", color=colors[n_order],
#                    label=r"$\mathcal{{O}}(\Delta x^{{{}}})$".format(2*order-2))
##    pyplot.plot(N, errs[n_order][len(N)-1]*(N[len(N)-1]/N)**4,
##                color="k", label=r"$\mathcal{O}(\Delta x^4)$")
#
#    ax = pyplot.gca()
#    ax.set_ylim(numpy.min(errs)/5, numpy.max(errs)*5)
#    ax.set_xscale('log')
#    ax.set_yscale('log')
#
#    pyplot.xlabel("N")
#    pyplot.ylabel(r"$\| a^\mathrm{final} - a^\mathrm{init} \|_2$",
#               fontsize=16)
#
#    pyplot.legend(frameon=False)
#    pyplot.savefig("weno-converge-sine-sine.pdf")
##    pyplot.show()
    
#-------------- RK4    
    
    problem = "gaussian"

    xmin = 0.0
    xmax = 1.0
    orders = [3, 5]
    N = [54, 64, 81, 108, 128]

    errs = []

    u = 1.0

    colors="brc"

    for order in orders:
        ng = order+1
        errs.append([])
        for nx in N:
            print(order, nx)
            gu = advection.Grid1d(nx, ng, xmin=xmin, xmax=xmax)
            su = WENOSimulation(gu, u, C=0.5, weno_order=order)
        
            su.init_cond("gaussian")
            ainit = su.grid.a.copy()
        
            su.evolve(num_periods=5)
        
            errs[-1].append(gu.norm(gu.a - ainit))
    
    pyplot.clf()
    N = numpy.array(N, dtype=numpy.float64)
    for n_order, order in enumerate(orders):
        pyplot.scatter(N, errs[n_order],
                       color=colors[n_order],
                       label=r"WENO, $r={}$".format(order))
    pyplot.plot(N, errs[0][-1]*(N[-1]/N)**(5),
                linestyle="--", color=colors[0],
                label=r"$\mathcal{{O}}(\Delta x^{{{}}})$".format(5))
    pyplot.plot(N, errs[n_order][len(N)-1]*(N[len(N)-1]/N)**4,
                color="k", label=r"$\mathcal{O}(\Delta x^4)$")

    ax = pyplot.gca()
    ax.set_ylim(numpy.min(errs)/5, numpy.max(errs)*5)
    ax.set_xscale('log')
    ax.set_yscale('log')

    pyplot.xlabel("N")
    pyplot.ylabel(r"$\| a^\mathrm{final} - a^\mathrm{init} \|_2$",
               fontsize=16)
    pyplot.title("Convergence of Gaussian, RK4")

    pyplot.legend(frameon=False)
    pyplot.savefig("weno-converge-gaussian-rk4.pdf")
#    pyplot.show()
    
#-------------- Gaussian    
    
    problem = "gaussian"

    xmin = 0.0
    xmax = 1.0
    orders = [3, 4, 5, 6]
    N = [24, 32, 54, 64, 81, 108, 128]
#    N = [32, 64, 108, 128]

    errs = []
    errsM = []

    u = 1.0

    colors="bygrc"

    for order in orders:
        ng = order+1
        errs.append([])
        errsM.append([])
        for nx in N:
            print(order, nx)
            gu = advection.Grid1d(nx, ng, xmin=xmin, xmax=xmax)
            su = WENOSimulation(gu, u, C=0.5, weno_order=order)
#            guM = advection.Grid1d(nx, ng, xmin=xmin, xmax=xmax)
#            suM = WENOMSimulation(guM, u, C=0.5, weno_order=order)
        
            su.init_cond("gaussian")
#            suM.init_cond("gaussian")
            ainit = su.grid.a.copy()
        
            su.evolve_scipy(num_periods=1)
#            suM.evolve_scipy(num_periods=1)
        
            errs[-1].append(gu.norm(gu.a - ainit))
#            errsM[-1].append(guM.norm(guM.a - ainit))
    
    pyplot.clf()
    N = numpy.array(N, dtype=numpy.float64)
    for n_order, order in enumerate(orders):
        pyplot.scatter(N, errs[n_order],
                       color=colors[n_order],
                       label=r"WENO, $r={}$".format(order))
#        pyplot.scatter(N, errsM[n_order],
#                       color=colors[n_order],
#                       label=r"WENOM, $r={}$".format(order))
        pyplot.plot(N, errs[n_order][0]*(N[0]/N)**(2*order-2),
                    linestyle="--", color=colors[n_order],
                    label=r"$\mathcal{{O}}(\Delta x^{{{}}})$".format(2*order-2))
#    pyplot.plot(N, errs[n_order][len(N)-1]*(N[len(N)-1]/N)**4,
#                color="k", label=r"$\mathcal{O}(\Delta x^4)$")

    ax = pyplot.gca()
    ax.set_ylim(numpy.min(errs)/5, numpy.max(errs)*5)
    ax.set_xscale('log')
    ax.set_yscale('log')

    pyplot.xlabel("N")
    pyplot.ylabel(r"$\| a^\mathrm{final} - a^\mathrm{init} \|_2$",
               fontsize=16)
    pyplot.title("Convergence of Gaussian, DOPRK8")

    pyplot.legend(frameon=False)
    pyplot.savefig("weno-converge-gaussian.pdf")
#    pyplot.show()
    