import numpy as np
import matplotlib.pyplot as plt

class FVGrid(object):

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
        self.a = self.scratch_array()
        self.ainit = self.scratch_array()

    def period(self, u):
        """ return the period for advection with velocity u """
        return (self.xmax - self.xmin)/u

    def scratch_array(self):
        """ return a scratch array dimensioned for our grid """
        return np.zeros((self.nx+2*self.ng), dtype=np.float64)

    def fill_BCs(self, atmp):
        """ fill all single ghostcell with periodic boundary conditions """

        # left boundary
        for n in range(self.ng):
            atmp[self.ilo-1-n] = atmp[self.ihi-n]

        # right boundary
        for n in range(self.ng):
            atmp[self.ihi+1+n] = atmp[self.ilo+n]

    def init_cond(self, ic):

        if ic == "tophat":
            self.a[np.logical_and(self.x >= 0.333, self.x <= 0.666)] = 1.0
        elif ic == "sine":
            self.a[:] = np.sin(2.0*np.pi*self.x/(self.xmax-self.xmin))
        elif ic == "gaussian":
            self.a[:] = 1.0 + np.exp(-60.0*(self.x - 0.5)**2)

        self.ainit[:] = self.a[:]

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))


def flux_update(gr, u, a, limit=False):

    if not limit:
        # slope
        da = gr.scratch_array()
        da[gr.ilo-1:gr.ihi+2] = 0.5*(a[gr.ilo:gr.ihi+3] - a[gr.ilo-2:gr.ihi+1])

    else:
        # MC slope
        ib = gr.ilo-1
        ie = gr.ihi+1

        dc = gr.scratch_array()
        dl = gr.scratch_array()
        dr = gr.scratch_array()

        dc[ib:ie+1] = 0.5*(a[ib+1:ie+2] - a[ib-1:ie  ])
        dl[ib:ie+1] = a[ib+1:ie+2] - a[ib  :ie+1]
        dr[ib:ie+1] = a[ib  :ie+1] - a[ib-1:ie  ]

        # these where's do a minmod()
        d1 = 2.0*np.where(np.fabs(dl) < np.fabs(dr), dl, dr)
        d2 = np.where(np.fabs(dc) < np.fabs(d1), dc, d1)
        da = np.where(dl*dr > 0.0, d2, 0.0)

    # upwinding means that we take the left state always
    aint = gr.scratch_array()
    aint[gr.ilo:gr.ihi+2] = a[gr.ilo-1:gr.ihi+1] + 0.5*da[gr.ilo-1:gr.ihi+1]

    flux_diff = gr.scratch_array()
    flux_diff[gr.ilo:gr.ihi+1] = u*(aint[gr.ilo:gr.ihi+1] - aint[gr.ilo+1:gr.ihi+2])/gr.dx

    return flux_diff


def mol_update(C, u, nx, num_periods=1, init_cond="gaussian", limit=False):

    # create a grid
    gr = FVGrid(nx, ng=2)

    tmax = num_periods*gr.period(u)

    # setup initial conditions
    gr.init_cond(init_cond)

    # compute the timestep
    dt = C*gr.dx/u

    t = 0.0
    while t < tmax:

        if t + dt > tmax:
            dt = tmax - t

        # second-order RK integration
        gr.fill_BCs(gr.a)
        k1 = flux_update(gr, u, gr.a, limit=limit)

        atmp = gr.scratch_array()
        atmp[:] = gr.a[:] + 0.5*dt*k1[:]

        gr.fill_BCs(atmp)
        k2 = flux_update(gr, u, atmp, limit=limit)

        gr.a[:] += dt*k2[:]

        t += dt

    return gr

if __name__ == "__main__":

    C = 0.8
    u = 1.0
    nx = 64

    # without limiting
    gr = mol_update(C, u, nx, num_periods=1.0, init_cond="gaussian")

    plt.clf()
    plt.plot(gr.x, gr.a)
    plt.plot(gr.x, gr.ainit, ls=":")
    plt.savefig("advection_gaussian.png", dpi=150)

    gr = mol_update(C, u, nx, num_periods=1.0, init_cond="tophat")

    plt.clf()
    plt.plot(gr.x, gr.a)
    plt.plot(gr.x, gr.ainit, ls=":")
    plt.savefig("advection_tophat.png", dpi=150)


    # with limiting
    gr = mol_update(C, u, nx, num_periods=1.0, init_cond="gaussian", limit=True)

    plt.clf()
    plt.plot(gr.x, gr.a)
    plt.plot(gr.x, gr.ainit, ls=":")
    plt.savefig("advection_gaussian_limit.png", dpi=150)

    gr = mol_update(C, u, nx, num_periods=1.0, init_cond="tophat", limit=True)

    plt.clf()
    plt.plot(gr.x, gr.a)
    plt.plot(gr.x, gr.ainit, ls=":")
    plt.savefig("advection_tophat_limit.png", dpi=150)


    # convergence
    plt.clf()

    Ns = [32, 64, 128, 256, 512]
    err_gauss = []
    err_nolimit = []
    for nx in Ns:
        gr = mol_update(C, u, nx, num_periods=1.0, init_cond="gaussian", limit=True)
        gr_nolimit = mol_update(C, u, nx, num_periods=1.0, init_cond="gaussian", limit=False)
        err_gauss.append(gr.norm(gr.a - gr.ainit))
        err_nolimit.append(gr.norm(gr_nolimit.a - gr_nolimit.ainit))
        print(nx, err_gauss[-1], err_nolimit[-1])

    plt.scatter(Ns, err_gauss, label="gaussian")
    plt.scatter(Ns, err_nolimit, label="gaussian (no limit)")

    # plot a trend line
    N = np.asarray(Ns)
    err = np.asarray(err_gauss)
    plt.plot(N, err[-1]*(N[-1]/N), ls=":", color="0.5", label=r"$\mathcal{O}(\Delta x)$")
    plt.plot(N, err[-1]*(N[-1]/N)**2, ls="-", color="0.5", label=r"$\mathcal{O}(\Delta x^2)$")

    ax = plt.gca()
    ax.set_xscale("log")
    ax.set_yscale("log")
    plt.legend(frameon=False, fontsize="small", loc=3)
    plt.xlim(10, 1000)
    plt.ylim(1.e-4, 0.1)

    plt.savefig("convergence_limited.png", dpi=150)
