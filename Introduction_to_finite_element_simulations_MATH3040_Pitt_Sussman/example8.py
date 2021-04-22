"""
example8.py: In parallel,
Poisson equation with Dirichlet and Neumann conditions.
Use of KrylovSolver, with non-zero initial guess
(non-zero initial guess might sometimes be useful) 
find max error

-Laplace(u) = f on the unit square.
u = 1 + 2y^2 on x=0.
u = 2 + 2y^2 on x=1.
-du/dn = g on y=0 and y=1.
u = 1 + x^2 + 2y^2, f = -6, g = -4y.
"""

from dolfin import *
import numpy as np

# Exact solution:
u_exact = Expression("1 + x[0]*x[0] + 2*x[1]*x[1]")

# Create mesh and define function space
mesh = UnitSquareMesh(20, 30)
V = FunctionSpace(mesh, "Lagrange", 1)

# Define Dirichlet conditions for x=0 boundary
u_L = Expression("1 + 2*x[1]*x[1]")

class LeftBoundary(SubDomain):
    def inside(self, x, on_boundary):
        tol = 1E-14   # tolerance for coordinate comparisons
        return on_boundary and abs(x[0]) < tol

Gamma_0 = DirichletBC(V, u_L, LeftBoundary())

# Define Dirichlet conditions for x=1 boundary
u_R = Expression("2 + 2*x[1]*x[1]")

class RightBoundary(SubDomain):
    def inside(self, x, on_boundary):
        tol = 1E-14   # tolerance for coordinate comparisons
        return on_boundary and abs(x[0] - 1) < tol
 
Gamma_1 = DirichletBC(V, u_R, RightBoundary())

bcs = [Gamma_0, Gamma_1]

# Define variational problem
u = TrialFunction(V)
v = TestFunction(V)
f = Constant(-6.0)
g = Expression("-4*x[1]")
a = inner(grad(u), grad(v))*dx
L = f*v*dx - g*v*ds

# Assemble and solve linear system
A, b = assemble_system(a, L, bcs)

# Compute solution
solver = KrylovSolver("cg", "bjacobi")
solver.parameters["absolute_tolerance"] = 1E-12
solver.parameters["relative_tolerance"] = 1E-9
solver.parameters["maximum_iterations"] = 10000
u = Function(V)
U = u.vector()

# What is this process number?
myID=MPI.process_number()

print "Local array size on process ", myID, " =", U.array().size
Uarr = np.random.uniform(-1000.0, 1000.0, U.array().size )
# here is how to set values into a PETSc array
locRange = U.local_range()
U[locRange[0]:locRange[1]] = Uarr
solver.parameters["nonzero_initial_guess"] = True
solver.solve(A, U, b)

if myID == 0:
    print """
    Solution of the Poisson problem -Laplace(u) = f,
    with u = u0 on x=0,1 and -du/dn = g at y=0,1.
    %s 
    process %d, using a random initial vector
    """ % (mesh, myID)

# Verification
u_array = U.array()
u_e = interpolate(u_exact, V)
u_e_array = u_e.vector().array()
localmax = np.amax( np.abs(u_e_array - u_array) )
globalmax = MPI.max(localmax)
if myID==0: 
    print "Max absolute error:", globalmax
