import numpy as np
import grid

class Multigrid:
    """
    The main multigrid class for cell-centered data.

    We require that nx be a power of 2 for simplicity
    """

    def __init__(self, nx, xmin=0.0, xmax=1.0,
                 bc_left_type="dirichlet", bc_right_type="dirichlet",
                 nsmooth=10, nsmooth_bottom=50,
                 verbose=0,
                 true_function=None):

        self.nx = nx
        self.ng = 1

        self.xmin = xmin
        self.xmax = xmax

        self.nsmooth = nsmooth
        self.nsmooth_bottom = nsmooth_bottom

        self.max_cycles = 100

        self.verbose = verbose

        self.bc_left_type = bc_left_type
        self.bc_right_type = bc_right_type

        # a function that gives the analytic solution (if available)
        # for diagnostics only
        self.true_function = true_function

        # assume that self.nx = 2^(nlevels-1)
        # this defines nlevels such that we end exactly on a 2 zone grid
        self.nlevels = int(np.log(self.nx)/np.log(2.0))

        # a multigrid object will be a list of grids
        self.grids = []

        # create the grids.  Here, self.grids[0] will be the coarsest
        # grid and self.grids[nlevel-1] will be the finest grid we
        # store the solution, v, the rhs, f.

        nx_t = 2
        for _ in range(self.nlevels):

            # add a grid for this level
            self.grids.append(grid.Grid(nx_t, xmin=self.xmin, xmax=self.xmax,
                                        bc_left_type=self.bc_left_type,
                                        bc_right_type=self.bc_right_type))

            nx_t *= 2

        # provide coordinate and indexing information for the solution mesh
        self.soln_grid = self.grids[self.nlevels-1]

        self.ilo = self.soln_grid.ilo
        self.ihi = self.soln_grid.ihi

        self.x = self.soln_grid.x
        self.dx = self.soln_grid.dx

        # store the source norm
        self.source_norm = 0.0

        # after solving, keep track of the number of cycles taken and
        # the residual error (normalized to the source norm)

        self.num_cycles = 0
        self.residual_error = 1.e33

    def get_solution(self):
        return self.grids[self.nlevels-1].v.copy()

    def get_solution_object(self):
        return self.grids[self.nlevels-1]

    def init_solution(self):
        """
        initialize the solution to the elliptic problem as zero
        """
        self.soln_grid.v[:] = 0.0

    def init_rhs(self, data):
        self.soln_grid.f[:] = data.copy()

        # store the source norm
        self.source_norm = self.soln_grid.norm(self.soln_grid.f)

    def smooth(self, level, nsmooth):
        """ use Gauss-Seidel iterations to smooth """

        myg = self.grids[level]

        myg.fill_bcs()

        # do red-black G-S
        for _ in range(nsmooth):

            myg.v[myg.ilo:myg.ihi+1:2] = 0.5 * (
                -myg.dx * myg.dx * myg.f[myg.ilo:myg.ihi+1:2] +
                myg.v[myg.ilo+1:myg.ihi+2:2] + myg.v[myg.ilo-1:myg.ihi:2])

            myg.fill_bcs()

            myg.v[myg.ilo+1:myg.ihi+1:2] = 0.5 * (
                -myg.dx * myg.dx * myg.f[myg.ilo+1:myg.ihi+1:2] +
                myg.v[myg.ilo+2:myg.ihi+2:2] + myg.v[myg.ilo:myg.ihi:2])

            myg.fill_bcs()

    def solve(self, rtol=1.e-11):
        """do V-cycles util the L2 norm of the relative solution difference is
        < rtol

        """

        if self.verbose:
            print("source norm = ", self.source_norm)

        residual_error = 1.e33
        cycle = 1

        # diagnostics that are returned -- residual error norm and true
        # error norm (if possible) for each cycle
        rlist = []
        elist = []

        while residual_error > rtol and cycle <= self.max_cycles:

            # zero out the solution on all but the finest grid
            for level in range(self.nlevels-1):
                self.grids[level].v[:] = 0.0

            # descending part
            if self.verbose:
                print(f"<<< beginning V-cycle (cycle {cycle}) >>>\n")

            self.v_cycle(self.nlevels-1)

            # compute the residual error, relative to the source norm
            residual_error = self.soln_grid.residual_norm()
            if self.source_norm != 0.0:
                residual_error /= self.source_norm

            if residual_error < rtol:
                self.num_cycles = cycle
                self.residual_error = residual_error
                self.soln_grid.fill_bcs()

            if self.verbose:
                print(f"cycle {cycle}: residual err / source norm = {residual_error:11.6g}\n")

            rlist.append(residual_error)

            if self.true_function is not None:
                elist.append(self.soln_grid.norm(self.soln_grid.v - self.true_function(self.soln_grid.x)))

            cycle += 1

        return elist, rlist

    def v_cycle(self, level):

        if level > 0:
            fp = self.grids[level]
            cp = self.grids[level-1]

            if self.verbose:
                old_res_norm = fp.residual_norm()

            # smooth on the current level
            self.smooth(level, self.nsmooth)

            # compute the residual
            fp.compute_residual()

            if self.verbose:
                print(f"  level = {level}, nx = {fp.nx:4}, residual change: {old_res_norm:11.6g} -> {fp.norm(fp.r):11.6g}")

            # restrict the residual down to the RHS of the coarser level
            cp.f[:] = fp.restrict("r")

            # solve the coarse problem
            self.v_cycle(level-1)

            # prolong the error up from the coarse grid
            fp.v += cp.prolong("v")

            if self.verbose:
                old_res_norm = fp.residual_norm()

            # smooth
            self.smooth(level, self.nsmooth)

            if self.verbose:
                print(f"  level = {level}, nx = {fp.nx:4}, residual change: {old_res_norm:11.6g} -> {fp.residual_norm():11.6g}")

        else:
            # solve the discrete coarse problem just via smoothing
            if self.verbose:
                print("  bottom solve")

            bp = self.grids[0]

            self.smooth(0, self.nsmooth_bottom)

            bp.fill_bcs()
