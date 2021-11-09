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
        self.v = self.scratch_array()
        self.f = self.scratch_array()
        self.r = self.scratch_array()

    def scratch_array(self):
        """return a scratch array dimensioned for our grid """
        return np.zeros((self.nx+2*self.ng), dtype=np.float64)

    def norm(self, e):
        """compute the L2 norm of e that lives on our grid"""
        return np.sqrt(self.dx * np.sum(e[self.ilo:self.ihi+1]**2))

    def compute_residual(self):
        """compute and store the residual"""
        self.r[self.ilo:self.ihi+1] = self.f[self.ilo:self.ihi+1] - \
            (self.v[self.ilo+1:self.ihi+2] -
             2 * self.v[self.ilo:self.ihi+1] +
             self.v[self.ilo-1:self.ihi]) / self.dx**2

    def residual_norm(self):
        """compute the residual norm"""
        self.compute_residual()
        return self.norm(self.r)

    def source_norm(self):
        """compute the source norm"""
        return self.norm(self.f)

    def fill_bcs(self):
        """fill the boundary conditions on phi"""

        # we only deal with a single ghost cell here

        # left
        if self.bc_left_type.lower() == "dirichlet":
            self.v[self.ilo-1] = 2 * self.bc_left_val - self.v[self.ilo]
        elif self.bc_left_type.lower() == "neumann":
            self.v[self.ilo-1] = self.v[self.ilo] - self.dx * self.bc_left_val
        else:
            raise ValueError("invalid bc_left_type")

        # right
        if self.bc_right_type.lower() == "dirichlet":
            self.v[self.ihi+1] = 2 * self.bc_right_val - self.v[self.ihi]
        elif self.bc_right_type.lower() == "neumann":
            self.v[self.ihi+1] = self.v[self.ihi] - self.dx * self.bc_right_val
        else:
            raise ValueError("invalid bc_right_type")

    def restrict(self, comp="v"):
        """restrict the data to a coarser (by 2x) grid"""

        # create a coarse array
        ng = self.ng
        nc = self.nx//2

        ilo_c = ng
        ihi_c = ng + nc - 1

        coarse_data = np.zeros((nc + 2*ng), dtype=np.float64)

        if comp == "v":
            fine_data = self.v
        elif comp == "f":
            fine_data = self.f
        elif comp == "r":
            fine_data = self.r
        else:
            raise ValueError("invalid component")

        coarse_data[ilo_c:ihi_c+1] = 0.5 * (fine_data[self.ilo:self.ihi+1:2] +
                                            fine_data[self.ilo+1:self.ihi+1:2])

        return coarse_data

    def prolong(self, comp="v"):
        """prolong the data in the current (coarse) grid to a finer (factor
        of 2 finer) grid using linear reconstruction.

        """

        if comp == "v":
            coarse_data = self.v
        elif comp == "f":
            coarse_data = self.f
        elif comp == "r":
            coarse_data = self.r
        else:
            raise ValueError("invalid component")


        # allocate an array for the coarsely gridded data
        ng = self.ng
        nf = self.nx * 2

        fine_data = np.zeros((nf + 2*ng), dtype=np.float64)

        ilo_f = ng
        ihi_f = ng + nf - 1

        # slopes for the coarse data
        m_x = self.scratch_array()
        m_x[self.ilo:self.ihi+1] = 0.5 * (coarse_data[self.ilo+1:self.ihi+2] -
                                          coarse_data[self.ilo-1:self.ihi])

        # fill the '1' children
        fine_data[ilo_f:ihi_f+1:2] = \
            coarse_data[self.ilo:self.ihi+1] - 0.25 * m_x[self.ilo:self.ihi+1]

        # fill the '2' children
        fine_data[ilo_f+1:ihi_f+1:2] = \
            coarse_data[self.ilo:self.ihi+1] + 0.25 * m_x[self.ilo:self.ihi+1]

        return fine_data
