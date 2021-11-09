import numpy as np
import matplotlib.pyplot as plt

import riemann

URHO = 0
UMX = 1
UENER = 2

QRHO = 0
QU = 1
QP = 2

NVAR = 3

class FVGrid(object):

    def __init__(self, nx, ng, xmin=0.0, xmax=1.0, bcs="outflow"):

        self.xmin = xmin
        self.xmax = xmax
        self.ng = ng
        self.nx = nx

        self.bcs = bcs

        # python is zero-based.  Make easy intergers to know where the
        # real data lives
        self.ilo = ng
        self.ihi = ng+nx-1

        # physical coords -- cell-centered, left and right edges
        self.dx = (xmax - xmin)/(nx)
        self.x = xmin + (np.arange(nx+2*ng)-ng+0.5)*self.dx
        self.xl = xmin + (np.arange(nx+2*ng)-ng)*self.dx
        self.xr = xmin + (np.arange(nx+2*ng)-ng+1.0)*self.dx

    def scratch_array(self, nc=1):
        """ return a scratch array dimensioned for our grid """
        if nc == 1:
            return np.zeros((self.nx+2*self.ng), dtype=np.float64)
        else:
            return np.zeros((self.nx+2*self.ng, nc), dtype=np.float64)

    def norm(self, e):
        """ return the norm of quantity e which lives on the grid """
        if not len(e) == (2*self.ng + self.nx):
            return None

        return np.sqrt(self.dx*np.sum(e[self.ilo:self.ihi+1]**2))

    def fill_BCs(self, atmp):
        """ fill all single ghostcell with periodic boundary conditions """

        try:
            nc = atmp.shape[1]
        except:
            nc = 1

        # outflow
        if self.bcs == "outflow":
            if nc == 1:
                atmp[0:self.ilo] = atmp[self.ilo]
                atmp[self.ihi+1:] = atmp[self.ihi]
            else:
                for n in range(nc):
                    atmp[0:self.ilo, n] = atmp[self.ilo, n]
                    atmp[self.ihi+1:, n] = atmp[self.ihi, n]

        elif self.bcs == "periodic":
            if nc == 1:
                for i in range(self.ng):
                    atmp[self.ilo-1-i] = atmp[self.ihi-i]
                    atmp[self.ihi+1+i] = atmp[self.ilo+i]

            else:
                for n in range(nc):
                    for i in range(self.ng):
                        atmp[self.ilo-1-i, n] = atmp[self.ihi-i, n]
                        atmp[self.ihi+1+i, n] = atmp[self.ilo+i, n]



class Simulation(object):

    def __init__(self, nx, params):

        self.params = params

        try:
            bcs = params["bcs"]
        except KeyError:
            bcs = "outflow"

        # create a grid
        self.gr = FVGrid(nx, ng=2, bcs=bcs)

    def cons_to_prim(self, U):

        q = self.gr.scratch_array(nc=NVAR)

        gamma = self.params['gamma']

        q[:, QRHO] = U[:, URHO]
        q[:, QU] = U[:, UMX]/U[:, URHO]
        q[:, QP] = (U[:, UENER] - 0.5*q[:, QRHO]*q[:, QU]**2)*(gamma - 1.0)

        return q

    def flux_update(self, U):

        gamma = self.params['gamma']

        # convert to primitive
        q = self.cons_to_prim(U)

        # construct the slopes
        dq = self.gr.scratch_array(nc=NVAR)

        for n in range(NVAR):
            a = q[:, n]

            # MC slope
            ib = self.gr.ilo-1
            ie = self.gr.ihi+1

            dc = self.gr.scratch_array()
            dl = self.gr.scratch_array()
            dr = self.gr.scratch_array()

            dc[ib:ie+1] = 0.5*(a[ib+1:ie+2] - a[ib-1:ie  ])
            dl[ib:ie+1] = a[ib+1:ie+2] - a[ib  :ie+1]
            dr[ib:ie+1] = a[ib  :ie+1] - a[ib-1:ie  ]

            # these where's do a minmod()
            d1 = 2.0*np.where(np.fabs(dl) < np.fabs(dr), dl, dr)
            d2 = np.where(np.fabs(dc) < np.fabs(d1), dc, d1)
            dq[:, n] = np.where(dl*dr > 0.0, d2, 0.0)
            #dq[:, n] = dc

        # now make the states
        q_l = self.gr.scratch_array(nc=NVAR)
        q_l[self.gr.ilo:self.gr.ihi+2, :] = q[self.gr.ilo-1:self.gr.ihi+1, :] + 0.5*dq[self.gr.ilo-1:self.gr.ihi+1, :]

        q_r = self.gr.scratch_array(nc=NVAR)
        q_r[self.gr.ilo:self.gr.ihi+2, :] = q[self.gr.ilo:self.gr.ihi+2, :] - 0.5*dq[self.gr.ilo:self.gr.ihi+2, :]

        # now solve the Riemann problem
        flux = self.gr.scratch_array(nc=NVAR)
        for i in range(self.gr.ilo, self.gr.ihi+2):
            flux[i, :] = riemann.riemann(q_l[i, :], q_r[i, :], gamma)

        A = self.gr.scratch_array(nc=NVAR)
        for n in range(NVAR):
            A[self.gr.ilo:self.gr.ihi+1, n] = (flux[self.gr.ilo:self.gr.ihi+1, n] -
                                               flux[self.gr.ilo+1:self.gr.ihi+2, n])/self.gr.dx

        return A

    def init_cond(self, U):

        idx_l = self.gr.x < 0.5
        idx_r = self.gr.x >= 0.5

        U[idx_l, URHO] = self.params['rho_l']
        U[idx_l, UMX] =  self.params['rho_l'] * self.params['u_l']
        U[idx_l, UENER] = self.params['p_l']/(self.params['gamma'] - 1.0) + 0.5 * self.params['rho_l'] * self.params['u_l']**2

        U[idx_r, URHO] = self.params['rho_r']
        U[idx_r, UMX] =  self.params['rho_r'] * self.params['u_r']
        U[idx_r, UENER] = self.params['p_r']/(self.params['gamma'] - 1.0) + 0.5 * self.params['rho_r'] * self.params['u_r']**2

    def timestep(self, U):

        # compute the sound speed
        q = self.cons_to_prim(U)
        c = self.gr.scratch_array()
        c[self.gr.ilo:self.gr.ihi+1] = np.sqrt(self.params['gamma'] *
                                               q[self.gr.ilo:self.gr.ihi+1,QP] /
                                               q[self.gr.ilo:self.gr.ihi+1,QRHO])

        dt = self.params['cfl'] * self.gr.dx / (np.abs(q[self.gr.ilo:self.gr.ihi+1, QU]) +
                                                c[self.gr.ilo:self.gr.ihi+1]).max()

        return dt


    def mol_update(self):

        U = self.gr.scratch_array(nc=NVAR)

        # setup initial conditions
        self.init_cond(U)

        t = 0.0
        tmax = self.params['tmax']

        try:
            verbose = self.params['verbose']
        except KeyError:
            verbose = 1

        istep = 0

        while t < tmax:

            # compute the timestep
            dt = self.timestep(U)

            if t + dt > tmax:
                dt = tmax - t

            # second-order RK integration
            self.gr.fill_BCs(U)
            k1 = self.flux_update(U)

            U_tmp = self.gr.scratch_array(nc=NVAR)
            for n in range(NVAR):
                U_tmp[:, n] = U[:, n] + 0.5 * dt * k1[:, n]

            self.gr.fill_BCs(U_tmp)
            k2 = self.flux_update(U_tmp)

            for n in range(NVAR):
                U[:, n] += dt * k2[:, n]

            t += dt
            istep += 1

            if verbose:
                print(istep, t, dt)

        return self.gr, U


if __name__ == "__main__":

    nx = 128

    # Sod's problem
    params = {
        'rho_l': 1.0,
        'u_l': 0.0,
        'p_l': 1.0,
        'rho_r': 0.125,
        'u_r': 0.0,
        'p_r': 0.1,
        'tmax': 0.2,
        'gamma': 1.4,
        'cfl': 0.8
        }

    sim = Simulation(nx, params)
    gr, U = sim.mol_update()

    exact = np.loadtxt("../sod_exact.out")

    plt.clf()
    plt.plot(gr.x, U[:, 0], "x")
    plt.plot(exact[:,0], exact[:,1])
    plt.savefig("sod.png", dpi=150)

    # double rarefaction
    params = {
        'rho_l': 1.0,
        'u_l': -2.0,
        'p_l': 0.4,
        'rho_r': 1.0,
        'u_r': 2.0,
        'p_r': 0.4,
        'tmax': 0.15,
        'gamma': 1.4,
        'cfl': 0.8
        }

    sim = Simulation(nx, params)
    gr, U = sim.mol_update()

    exact = np.loadtxt("../double_rarefaction_exact.out")

    plt.clf()
    plt.plot(gr.x, U[:, 0], "x")
    plt.plot(exact[:,0], exact[:,1])
    plt.savefig("double_rarefaction.png", dpi=150)

    # strong shock
    params = {
        'rho_l': 1.0,
        'u_l': 0.0,
        'p_l': 1000.0,
        'rho_r': 1.0,
        'u_r': 0.0,
        'p_r': 0.01,
        'tmax': 0.012,
        'gamma': 1.4,
        'cfl': 0.8
        }

    sim = Simulation(nx, params)
    gr, U = sim.mol_update()

    exact = np.loadtxt("../strong_shock_exact.out")

    plt.clf()
    plt.plot(gr.x, U[:, 0], "x")
    plt.plot(exact[:,0], exact[:,1])
    plt.savefig("strong_shock.png", dpi=150)

    # slow moving shock
    params = {
        'rho_l': 5.6698,
        'u_l': -1.4701,
        'p_l': 100.0,
        'rho_r': 1.0,
        'u_r': -10.5,
        'p_r': 1.0,
        'tmax': 1.0,
        'gamma': 1.4,
        'cfl': 0.8
        }

    sim = Simulation(nx, params)
    gr, U = sim.mol_update()

    exact = np.loadtxt("../slow_shock_exact.out")

    plt.clf()
    plt.plot(gr.x, U[:, 0], "x")
    plt.plot(exact[:,0], exact[:,1])
    plt.savefig("slow_shock.png", dpi=150)
