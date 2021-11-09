import numpy as np

class Grid:
    def __init__(self, nx, ng=1, xmin=0, xmax=1,
                 bc_left_type="dirichlet", bc_left_val=0.0,
                 bc_right_type="dirichlet", bc_right_val=0.0):

        self.xmin = xmin
        self.xmax = xmax
        self.ng = ng
        self.nx = nx

        self.bc_left_type = bc_left_type
        self.bc_left_val = bc_left_val

        self.bc_right_type = bc_right_type
        self.bc_right_val = bc_right_val

        # python is zero-based.  Make easy intergers to know where the
        # real data lives
        self.ilo = ng
        self.ihi = ng+nx-1

        # physical coords -- cell-centered
        self.dx = (xmax - xmin)/(nx)
        self.x = xmin + (np.arange(nx+2*ng)-ng+0.5)*self.dx

        # storage for the solution
        self.phi = self.scratch_array()
        self.f = self.scratch_array()

    def scratch_array(self):
        """return a scratch array dimensioned for our grid """
        return np.zeros((self.nx+2*self.ng), dtype=np.float64)

    def norm(self, e):
        """compute the L2 norm of e that lives on our grid"""
        return np.sqrt(self.dx * np.sum(e[self.ilo:self.ihi+1]**2))

    def residual_norm(self):
        """compute the residual norm"""
        r = self.scratch_array()
        r[self.ilo:self.ihi+1] = self.f[self.ilo:self.ihi+1] - (self.phi[self.ilo+1:self.ihi+2] -
                                                                2 * self.phi[self.ilo:self.ihi+1] +
                                                                self.phi[self.ilo-1:self.ihi]) / self.dx**2
        return self.norm(r)

    def source_norm(self):
        """compute the source norm"""
        return self.norm(self.f)

    def fill_bcs(self):
        """fill the boundary conditions on phi"""

        # we only deal with a single ghost cell here

        # left
        if self.bc_left_type.lower() == "dirichlet":
            self.phi[self.ilo-1] = 2 * self.bc_left_val - self.phi[self.ilo]
        elif self.bc_left_type.lower() == "neumann":
            self.phi[self.ilo-1] = self.phi[self.ilo] - self.dx * self.bc_left_val
        else:
            raise ValueError("invalid bc_left_type")

        # right
        if self.bc_right_type.lower() == "dirichlet":
            self.phi[self.ihi+1] = 2 * self.bc_right_val - self.phi[self.ihi]
        elif self.bc_right_type.lower() == "neumann":
            self.phi[self.ihi+1] = self.phi[self.ihi] - self.dx * self.bc_right_val
        else:
            raise ValueError("invalid bc_right_type")

class TooManyIterations(Exception):
    pass

def relax(g, tol=1.e-8, max_iters=200000, analytic=None):

    iter = 0
    fnorm = g.source_norm()
    if fnorm == 0.0:
        fnorm = tol

    r = g.residual_norm()

    res_norm = []
    true_norm = []

    if tol is None:
        test = iter < max_iters
    else:
        test = iter < max_iter and r > tol * fnorm

    g.fill_bcs()

    while test:
        g.phi[g.ilo:g.ihi+1:2] = 0.5 * (-g.dx * g.dx * g.f[g.ilo:g.ihi+1:2] +
                                        g.phi[g.ilo+1:g.ihi+2:2] + g.phi[g.ilo-1:g.ihi:2])

        g.fill_bcs()

        g.phi[g.ilo+1:g.ihi+1:2] = 0.5 * (-g.dx * g.dx * g.f[g.ilo+1:g.ihi+1:2] +
                                          g.phi[g.ilo+2:g.ihi+2:2] + g.phi[g.ilo:g.ihi:2])

        g.fill_bcs()

        r = g.residual_norm()
        res_norm.append(r)

        if analytic is not None:
            true_norm.append(g.norm(g.phi - analytic(g.x)))

        iter += 1

        if tol is None:
            test = iter < max_iters
        else:
            test = iter < max_iter and r > tol * fnorm

    if tol is not None and iter >= max_iters:
        raise TooManyIterations(f"too many iteration, niter = {iter}")

    return res_norm, true_norm

