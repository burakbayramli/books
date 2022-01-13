"""
The multigrid module provides a framework for solving elliptic
problems.  A multigrid object is just a list of grids, from the finest
mesh down (by factors of two) to a single interior zone (each grid has
the same number of guardcells).

The main multigrid class is setup to solve a constant-coefficient
Helmholtz equation:

(alpha - beta L) phi = f

where L is the Laplacian and alpha and beta are constants.  If alpha =
0 and beta = -1, then this is the Poisson equation.

We support homogeneous Dirichlet or Neumann BCs, or on periodic domain.

The general usage is as follows:

> a = multigrid.CellCenterMG1d(nx, verbose=1, alpha=alpha, beta=beta)

this creates the multigrid object a, with a finest grid of nx zones
and the default boundary condition types.  alpha and beta are the
coefficients of the Helmholtz equation.  Setting verbose = 1 causing
debugging information to be output, so you can see the residual errors
in each of the V-cycles.

> a.init_zeros()

this initializes the solution vector with zeros

> a.initRHS(zeros((nx), numpy.float64))

this initializes the RHS on the finest grid to 0 (Laplace's equation).
Any RHS can be set by passing through an array of nx values here.

Then to solve, you just do:

> a.solve(rtol = 1.e-10)

where rtol is the desired tolerance (residual norm / source norm)

to access the final solution, use the getSolution method

v = a.get_solution()

For convenience, the grid information on the solution level is available as
attributes to the class,

a.ilo, a.ihi are the indices bounding the interior
of the solution array (i.e. excluding the guardcells).

a.x is the coordinate arrays, a.dx is the grid spacings

M. Zingale 

"""

from __future__ import print_function

import math

import numpy
import sys

import patch1d

def _error(myg, r):

    # L2 norm of elements in r, multiplied by dx to normalize
    return numpy.sqrt(myg.dx*numpy.sum((r[myg.ilo:myg.ihi+1]**2)) )


class CellCenterMG1d:
    """ 
    The main multigrid class for cell-centered data.

    We require that nx be a power of 2 for simplicity
    """
    
    def __init__(self, nx, xmin=0.0, xmax=1.0, 
                 xl_BC_type="dirichlet", xr_BC_type="dirichlet",
                 alpha=0.0, beta=-1.0,
                 nsmooth=10, nsmooth_bottom=50,
                 verbose=0, 
                 true_function=None):
        
        self.nx = nx
        self.ng = 1

        self.xmin = xmin
        self.xmax = xmax
        
        self.alpha = alpha
        self.beta = beta

        self.nsmooth = nsmooth
        self.nsmooth_bottom = nsmooth_bottom

        self.max_cycles = 100
        
        self.verbose = verbose


        # a function that gives the analytic solution (if available)
        # for diagnostics only
        self.true_function = true_function

        # a small number used in computing the error, so we don't divide by 0
        self.small = 1.e-16
        
        # keep track of whether we've initialized the solution
        self.initialized_solution = 0
        self.initialized_RHS = 0
        
        # assume that self.nx = 2^(nlevels-1)
        # this defines nlevels such that we end exactly on a 2 zone grid
        self.nlevels = int(math.log(self.nx)/math.log(2.0)) 

        # a multigrid object will be a list of grids
        self.grids = []

        # create the grids.  Here, self.grids[0] will be the coarsest
        # grid and self.grids[nlevel-1] will be the finest grid
        # we store the solution, v, the rhs, f.

        if self.verbose:
            print("alpha = ", self.alpha)
            print("beta  = ", self.beta)

        nx_t = 2
        for i in range(self.nlevels):
            
            # create the grid
            my_grid = patch1d.Grid1d(nx_t, ng=self.ng,
                                    xmin=xmin, xmax=xmax)

            # add a CellCenterData1d object for this level to our list
            self.grids.append(patch1d.CellCenterData1d(my_grid, dtype=numpy.float64))

            # create the boundary condition object
            bc = patch1d.BCObject(xlb=xl_BC_type, xrb=xr_BC_type)

            self.grids[i].register_var("v", bc)
            self.grids[i].register_var("f", bc)
            self.grids[i].register_var("r", bc)

            self.grids[i].create()

            if self.verbose:
                print(self.grids[i])

            nx_t = nx_t*2

        # provide coordinate and indexing information for the solution mesh
        soln_grid = self.grids[self.nlevels-1].grid

        self.ilo = soln_grid.ilo
        self.ihi = soln_grid.ihi
        
        self.x  = soln_grid.x
        self.dx = soln_grid.dx

        self.soln_grid = soln_grid

        # store the source norm
        self.source_norm = 0.0

        # after solving, keep track of the number of cycles taken, the
        # relative error from the previous cycle, and the residual error
        # (normalized to the source norm)
        self.num_cycles = 0
        self.residualError = 1.e33
        self.relativeError = 1.e33

        
    def get_solution(self):
        v = self.grids[self.nlevels-1].get_var("v")
        return v.copy()
        

    def get_solution_object(self):
        return self.grids[self.nlevels-1]


    def init_solution(self, data):
        """
        initialize the solution to the elliptic problem by passing in
        a value for all defined zones
        """
        v = self.grids[self.nlevels-1].get_var("v")
        v[:] = data.copy()

        self.initialized_solution = 1


    def init_zeros(self):
        """
        set the initial solution to zero
        """
        v = self.grids[self.nlevels-1].get_var("v")
        v[:] = 0.0

        self.initialized_solution = 1


    def init_RHS(self, data):
        f = self.grids[self.nlevels-1].get_var("f")
        f[:] = data.copy()

        # store the source norm
        self.source_norm = _error(self.grids[self.nlevels-1].grid, f)

        if self.verbose:
            print("Source norm = ", self.source_norm)

        # note: if we wanted to do inhomogeneous Dirichlet BCs, we 
        # would modify the source term, f, here to include a boundary
        # charge

        self.initialized_RHS = 1
        

    def _compute_residual(self, level):
        """ compute the residual and store it in the r variable"""

        v = self.grids[level].get_var("v")
        f = self.grids[level].get_var("f")
        r = self.grids[level].get_var("r")

        myg = self.grids[level].grid

        # compute the residual 
        # r = f - alpha phi + beta L phi
        r[myg.ilo:myg.ihi+1] = \
            f[myg.ilo:myg.ihi+1] - self.alpha*v[myg.ilo:myg.ihi+1] + \
            self.beta*( (v[myg.ilo-1:myg.ihi  ] + v[myg.ilo+1:myg.ihi+2] - 
                         2.0*v[myg.ilo:myg.ihi+1])/(myg.dx*myg.dx) )

        
    def smooth(self, level, nsmooth):
        """ use Gauss-Seidel iterations to smooth """
        v = self.grids[level].get_var("v")
        f = self.grids[level].get_var("f")

        myg = self.grids[level].grid

        self.grids[level].fill_BC("v")

        # do red-black G-S
        for i in range(nsmooth):
            xcoeff = self.beta/myg.dx**2

            # do the red black updating in four decoupled groups
            v[myg.ilo:myg.ihi+1:2] = \
                (f[myg.ilo:myg.ihi+1:2] +
                 xcoeff*(v[myg.ilo+1:myg.ihi+2:2] + v[myg.ilo-1:myg.ihi  :2])) / \
                 (self.alpha + 2.0*xcoeff)
            
            self.grids[level].fill_BC("v")
                                                     
            v[myg.ilo+1:myg.ihi+1:2] = \
                (f[myg.ilo+1:myg.ihi+1:2] +
                 xcoeff*(v[myg.ilo+2:myg.ihi+2:2] + v[myg.ilo  :myg.ihi  :2])) / \
                 (self.alpha + 2.0*xcoeff)

            self.grids[level].fill_BC("v")


    def solve(self, rtol = 1.e-11):

        # start by making sure that we've initialized the solution
        # and the RHS
        if not self.initialized_solution or not self.initialized_RHS:
            sys.exit("ERROR: solution and RHS are not initialized")

        # for now, we will just do V-cycles, continuing until we
        # achieve the L2 norm of the relative solution difference is <
        # rtol
        if self.verbose:
            print("source norm = ", self.source_norm)
            
        old_solution = self.grids[self.nlevels-1].get_var("v").copy()
        
        converged = 0
        cycle = 1

        # diagnostics that are returned -- residual error norm and true
        # error norm (if possible) for each cycle
        rlist = []
        elist = []

        while not converged and cycle <= self.max_cycles:

            # zero out the solution on all but the finest grid
            for level in range(self.nlevels-1):
                v = self.grids[level].zero("v")

            # descending part
            if self.verbose:
                print("<<< beginning V-cycle (cycle {}) >>>\n".format(cycle))

            level = self.nlevels-1
            while level > 0:

                fP = self.grids[level]
                cP = self.grids[level-1]

                # access to the residual
                r = fP.get_var("r")

                if self.verbose:
                    self._compute_residual(level)

                    print("  level = {}, nx = {}".format(level, fP.grid.nx))
                    print("  before G-S, residual L2 norm = {}".format(_error(fP.grid, r) ))
            
                # smooth on the current level
                self.smooth(level, self.nsmooth)

            
                # compute the residual
                self._compute_residual(level)

                if self.verbose:
                    print("  after G-S, residual L2 norm = {}\n".format(_error(fP.grid, r) ))


                # restrict the residual down to the RHS of the coarser level
                f_coarse = cP.get_var("f")
                f_coarse[:] = fP.restrict("r")

                level -= 1


            # solve the discrete coarse problem.  We could use any
            # number of different matrix solvers here (like CG), but
            # since we are 2 zone by design at this point, we will
            # just smooth
            if self.verbose:
                print("  bottom solve:")

            bP = self.grids[0]

            if self.verbose:
                print("  level = {}, nx = {}\n".format(level, bP.grid.nx))

            self.smooth(0, self.nsmooth_bottom)

            bP.fill_BC("v")

            
            # ascending part
            for level in range(1, self.nlevels):

                fP = self.grids[level]
                cP = self.grids[level-1]

                # prolong the error up from the coarse grid
                e = cP.prolong("v")

                # correct the solution on the current grid
                v = fP.get_var("v")
                v += e

                if self.verbose:
                    self._compute_residual(level)
                    r = fP.get_var("r")

                    print("  level = {}, nx = {}".format(level, fP.grid.nx))

                    print("  before G-S, residual L2 norm = {}".format(_error(fP.grid, r) ))
            
                # smooth
                self.smooth(level, self.nsmooth)

                if self.verbose:
                    self._compute_residual(level)

                    print("  after G-S, residual L2 norm = {}\n".format(_error(fP.grid, r) ))
            

            # compute the error with respect to the previous solution
            # this is for diagnostic purposes only -- it is not used to
            # determine convergence
            solnP = self.grids[self.nlevels-1]

            diff = (solnP.get_var("v") - old_solution)/ \
                (solnP.get_var("v") + self.small)

            relative_error = _error(solnP.grid, diff)

            old_solution = solnP.get_var("v").copy()

            # compute the residual error, relative to the source norm
            self._compute_residual(self.nlevels-1)
            r = fP.get_var("r")

            if self.source_norm != 0.0:
                residual_error = _error(fP.grid, r)/self.source_norm
            else:
                residual_error = _error(fP.grid, r)

            if residual_error < rtol:
                converged = 1
                self.num_cycles = cycle
                self.relative_error = relative_error
                self.residual_error = residual_error
                fP.fill_BC("v")
                
            if self.verbose:
                print("cycle {}: relative err = {}, residual err = {}\n".format(
                    cycle, relative_error, residual_error))

            rlist.append(residual_error)
            
            if not self.true_function == None:
                elist.append(_error(fP.grid, (old_solution - self.true_function(fP.grid.x))))

            cycle += 1
        

        return elist, rlist


        
        
