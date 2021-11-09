# this solves the 1-d spherical Euler equations, using a
# method-of-lines integration approach, building on the 1-d solver we
# did in the tutorial

import matplotlib.pyplot as plt
import numpy as np
import riemann_exact as re
import riemann_approximate as ra

import sys

class FluidVars:
    """A simple container that holds the integer indicies we will use to
    refer to the different fluid components"""
    def __init__(self, gamma=1.4, C=0.8):
        self.nvar = 3

        # conserved variables
        self.urho = 0
        self.umx = 1
        self.uener = 2

        # primitive variables
        self.qrho = 0
        self.qu = 1
        self.qp = 2

        # EOS gamma
        self.gamma = gamma

        # CFL number
        self.C = C

class FVGrid:
    """The main finite-volume grid class for holding our fluid state."""

    def __init__(self, nr, ng, rmin=0.0, rmax=1.0, fvars=None,
                 small_dens=1.e-10, small_pres=1.e-20):

        self.small_dens = small_dens
        self.small_pres = small_pres

        self.rmin = rmin
        self.rmax = rmax
        self.ng = ng
        self.nr = nr

        self.lo = ng
        self.hi = ng+nr-1

        # physical coords -- cell-centered
        self.dr = (rmax - rmin)/(nr)
        self.rl = rmin + (np.arange(nr+2*ng)-ng)*self.dr
        self.rr = rmin + (np.arange(nr+2*ng)-ng+1.0)*self.dr
        self.r = rmin + (np.arange(nr+2*ng)-ng+0.5)*self.dr

        self.v = fvars

    def scratch_array(self, nc=1):
        """ return a scratch array dimensioned for our grid """
        return np.squeeze(np.zeros((self.nr+2*self.ng, nc), dtype=np.float64))

    def fill_BCs(self, atmp):
        """we do reflecting at r = 0 and outflow at r = rmax"""

        # lower boundary
        for i in range(self.ng):
            atmp[self.lo-1-i, self.v.urho] = atmp[self.lo+i, self.v.urho]
            atmp[self.lo-1-i, self.v.umx] = -atmp[self.lo+i, self.v.umx]
            atmp[self.lo-1-i, self.v.uener] = atmp[self.lo+i, self.v.uener]

        for n in range(self.v.nvar):
            atmp[self.hi+1:, n] = atmp[self.hi, n]

def cons_to_prim(g, U, ):
    """take a conservative state U and return the corresponding primitive
    variable state as a new array."""
    q = g.scratch_array(nc=g.v.nvar)

    q[:, g.v.qrho] = np.maximum(U[:, g.v.urho], g.small_dens)
    q[:, g.v.qu] = U[:, g.v.umx]/U[:, g.v.urho]
    rhoe = U[:, g.v.uener] - 0.5*q[:, g.v.qrho]*q[:, g.v.qu]**2
    q[:, g.v.qp] = np.maximum(rhoe*(g.v.gamma - 1.0), g.small_pres)

    return q

def states(g, U):

    q = cons_to_prim(g, U)

    # construct the slopes
    dq = g.scratch_array(nc=g.v.nvar)

    for n in range(g.v.nvar):
        dl = g.scratch_array()
        dr = g.scratch_array()
        dc = g.scratch_array()

        dl[g.lo-1:g.hi+2] = q[g.lo-1:g.hi+2,n] - q[g.lo-2:g.hi+1,n]
        dr[g.lo-1:g.hi+2] = q[g.lo:g.hi+3,n] - q[g.lo-1:g.hi+2,n]
        dc[g.lo-1:g.hi+2] = 0.5 * (q[g.lo:g.hi+3,n] - q[g.lo-2:g.hi+1,n])

        # these where's do a minmod()
        d1 = 2.0 * np.where(np.fabs(dl) < np.fabs(dr), dl, dr)
        d2 = np.where(np.fabs(dc) < np.fabs(d1), dc, d1)
        dq[:, n] = np.where(dl*dr > 0.0, d2, 0.0)

    # now make the states
    q_l = g.scratch_array(nc=g.v.nvar)
    q_l[g.lo:g.hi+2, :] = q[g.lo-1:g.hi+1, :] + 0.5*dq[g.lo-1:g.hi+1, :]

    q_r = g.scratch_array(nc=g.v.nvar)
    q_r[g.lo:g.hi+2, :] = q[g.lo:g.hi+2, :] - 0.5*dq[g.lo:g.hi+2, :]

    return q_l, q_r

def cons_flux(state, v):
    """ given an interface state, return the conservative flux"""
    flux = np.zeros((v.nvar), dtype=np.float64)

    flux[v.urho] = state.rho * state.u

    # note in spherical coords, we don't add the pressure to the
    # momentum flux, because it is not a divergence
    flux[v.umx] = flux[v.urho] * state.u

    flux[v.uener] = (0.5 * state.rho * state.u**2 +
                     state.p/(v.gamma - 1.0) + state.p) * state.u
    return flux

def make_flux_divergence(g, U):

    # get the states
    q_l, q_r = states(g, U)

    # now solve the Riemann problem
    flux = g.scratch_array(nc=g.v.nvar)

    # store the interface pressure for the pressure gradient
    p_int = g.scratch_array()

    for i in range(g.lo, g.hi+2):
        sl = ra.State(rho=q_l[i, g.v.qrho], u=q_l[i, g.v.qu], p=q_l[i, g.v.qp])
        sr = ra.State(rho=q_r[i, g.v.qrho], u=q_r[i, g.v.qu], p=q_r[i, g.v.qp])

        #rp = re.RiemannProblem(sl, sr, gamma=g.v.gamma)
        #rp.find_2shock_star_state(p_min=1.e-10, p_max=1.e10)
        #q_int = rp.sample_solution()

        q_int = ra.riemann(sl, sr, g.v.gamma)
        flux[i, :] = cons_flux(q_int, g.v)
        p_int[i] = q_int.p
        if (p_int[i] < 0):
            sys.exit(f"p < 0 at i = {i}")
        if (i == g.lo):
            flux[i, :] = 0.0

    A = g.scratch_array(nc=g.v.nvar)
    for n in range(g.v.nvar):
        A[g.lo:g.hi+1, n] = (1/g.r[g.lo:g.hi+1]**2) * (
            g.rl[g.lo:g.hi+1]**2 * flux[g.lo:g.hi+1, n] -
            g.rr[g.lo+1:g.hi+2]**2 * flux[g.lo+1:g.hi+2, n]) / g.dr

    # now add the pressure gradient

    A[g.lo:g.hi+1, g.v.umx] -= (p_int[g.lo+1:g.hi+2] - p_int[g.lo:g.hi+1]) / g.dr

    return A

def timestep(g, U):

    # compute the sound speed
    q = cons_to_prim(g, U)
    c = g.scratch_array()
    c[g.lo:g.hi+1] = np.sqrt(g.v.gamma *
                             q[g.lo:g.hi+1, g.v.qp] /
                             q[g.lo:g.hi+1, g.v.qrho])

    dt = g.v.C * g.dr / (np.abs(q[g.lo:g.hi+1, g.v.qu]) +
                         c[g.lo:g.hi+1]).max()
    return dt

def mol_solve(nx, C=0.4, init_shrink=0.01, change_max=1.1,
              tmax=1.0, init_cond=None, method="ssprk3"):
    """Perform 2nd order MOL integration of the Euler equations.
    You need to pass in a function foo(grid) that returns the
    initial conserved fluid state."""

    v = FluidVars(C=C)

    grid = FVGrid(nx, 2, fvars=v)

    U = init_cond(grid)

    t = 0.0

    dt = init_shrink * timestep(grid, U)

    while t < tmax:

        q = cons_to_prim(grid, U)
        print("  rho: ", q[grid.lo:grid.hi+1, grid.v.qrho].min(), q[grid.lo:grid.hi+1, grid.v.qrho].max())
        print("  u:   ", q[grid.lo:grid.hi+1, grid.v.qu].min(), q[grid.lo:grid.hi+1, grid.v.qu].max())
        print("  p:   ", q[grid.lo:grid.hi+1, grid.v.qp].min(), q[grid.lo:grid.hi+1, grid.v.qp].max())

        if t + dt > tmax:
            dt = tmax - t


        if method == "rk2":

            grid.fill_BCs(U)

            k1 = make_flux_divergence(grid, U)

            U_tmp = grid.scratch_array(nc=v.nvar)
            for n in range(v.nvar):
                U_tmp[:, n] = U[:, n] + 0.5 * dt * k1[:, n]

            grid.fill_BCs(U_tmp)

            k2 = make_flux_divergence(grid, U_tmp)

            for n in range(v.nvar):
                U[:, n] += dt * k2[:, n]

        elif method == "ssprk3":

            grid.fill_BCs(U)
            k1 = make_flux_divergence(grid, U)

            U1 = grid.scratch_array(nc=v.nvar)
            for n in range(v.nvar):
                U1[:, n] = U[:, n] + dt * k1[:, n]

            grid.fill_BCs(U1)

            k2 = make_flux_divergence(grid, U1)

            U2 = grid.scratch_array(nc=v.nvar)
            for n in range(v.nvar):
                U2[:,n] = U[:,n] + 0.25 * dt * k1[:,n] + 0.25 * dt * k2[:,n]

            grid.fill_BCs(U2)

            k3 = make_flux_divergence(grid, U2)

            for n in range(v.nvar):
                U[:,n] += (dt/6) * k1[:,n] + (dt/6) * k2[:,n] + (2*dt/3) * k3[:,n]

        elif method == "rk4":

            U_tmp = grid.scratch_array(nc=v.nvar)

            grid.fill_BCs(U)
            k1 = make_flux_divergence(grid, U)

            for n in range(v.nvar):
                U_tmp[:, n] = U[:, n] + 0.5 * dt * k1[:, n]

            grid.fill_BCs(U_tmp)
            k2 = make_flux_divergence(grid, U_tmp)

            for n in range(v.nvar):
                U_tmp[:, n] = U[:, n] + 0.5 * dt * k2[:, n]

            grid.fill_BCs(U_tmp)
            k3 = make_flux_divergence(grid, U_tmp)

            for n in range(v.nvar):
                U_tmp[:, n] = U[:, n] + dt * k3[:, n]

            grid.fill_BCs(U_tmp)
            k4 = make_flux_divergence(grid, U_tmp)

            for n in range(v.nvar):
                U[:, n] += dt/6.0 * (k1[:, n] + 2.0*k2[:, n] + 2.0*k3[:, n] + k4[:, n])



        #for n in range(v.nvar):
        #    U[:, n] += dt * k1[:, n]

        # prevent rho from being negative
        #np.clip(U[:, grid.v.qrho], 1.e-10, None)

        t += dt
        print(t)

        dt = min(1.1*dt, timestep(grid, U))


    return grid, U

def sedov(g):

    U = g.scratch_array(nc=g.v.nvar)

    # setup initial conditions -- this is Sod's problem
    rho_ambient = 1.0
    p_ambient = 1.e-5
    u_ambient = 0.0

    r_0 = 1.0/128

    E_expl = 1.0
    p_expl = (g.v.gamma - 1.0) * E_expl / (4.0/3.0 * np.pi * r_0**3)

    idx_l = g.r < r_0
    idx_r = g.r >= r_0

    U[idx_l, g.v.urho] = rho_ambient
    U[idx_l, g.v.umx] =  rho_ambient * u_ambient
    U[idx_l, g.v.uener] = p_expl / (g.v.gamma - 1.0) + 0.5 * rho_ambient * u_ambient**2

    U[idx_r, g.v.urho] = rho_ambient
    U[idx_r, g.v.umx] =  rho_ambient * u_ambient
    U[idx_r, g.v.uener] = p_ambient / (g.v.gamma - 1.0) + 0.5 * rho_ambient * u_ambient**2

    return U

if __name__ == "__main__":

    g, U = mol_solve(256, C=0.4, tmax=0.01, init_cond=sedov, method="rk4")

    print(g.rl[g.lo:g.lo+4])
    print(g.r[g.lo:g.lo+4])
    print(g.rr[g.lo:g.lo+4])

    sod_exact = np.genfromtxt("sod-exact.out", skip_header=2, names=True)

    v = FluidVars()

    q = cons_to_prim(g, U)
    fig = plt.figure()

    ax = fig.add_subplot(311)
    ax.plot(g.r[g.lo:g.hi+1], q[g.lo:g.hi+1,v.qrho], marker="x", color="C0")
    #ax.plot(sod_exact["x"], sod_exact["rho"], color="C1")

    ax = fig.add_subplot(312)
    ax.plot(g.r[g.lo:g.hi+1], q[g.lo:g.hi+1,v.qu], marker="x", color="C0")
    #ax.plot(sod_exact["x"], sod_exact["u"], color="C1")

    ax = fig.add_subplot(313)
    ax.plot(g.r[g.lo:g.hi+1], q[g.lo:g.hi+1,v.qp], marker="x", color="C0")
    #ax.plot(sod_exact["x"], sod_exact["p"], color="C1")

    fig.set_size_inches((6, 10))
    fig.savefig("test.png")
