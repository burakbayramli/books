"""
example13.py,
Nonlinear 1D Poisson equation with Dirichlet conditions

-div(q(u)*nabla_grad(u)) = 0,
u = 0 at x=0, u=1 at x=1, q(u) = (1+u)^m

Solution method: automatic, i.e., by a NonlinearVariationalProblem/Solver
(Newton method), with automatic UFL computation of the derivative.
"""
from dolfin import *
import numpy

# Create mesh and define function space
mesh = UnitIntervalMesh(20)
V = FunctionSpace(mesh, "Lagrange", 1)

# Define boundary conditions
fuzz = 1E-14
def left_boundary(x, on_boundary):
    return on_boundary and abs(x[0]) < fuzz

def right_boundary(x, on_boundary):
    return on_boundary and abs(x[0]-1) < fuzz

Gamma_0 = DirichletBC(V, Constant(0.0), left_boundary)
Gamma_1 = DirichletBC(V, Constant(1.0), right_boundary)
bcs = [Gamma_0, Gamma_1]

# Choice of nonlinear coefficient
m = 2
def q(u):
    return (1+u)**m

# Define variational problem
v   = TestFunction(V)
u   = TrialFunction(V)
F   = inner(q(u)*nabla_grad(u), nabla_grad(v))*dx
u_  = Function(V)

# make functional into a vector function
F   = action(F, u_ )

# Automatic differentiation
J = derivative(F, u_ )

# set initial guess
# u_  is zero by default
uinit = interpolate(Expression("2.*x[0]*x[0]"),V)
u_.assign(uinit)

# Compute solution
problem = NonlinearVariationalProblem(F, u_ , bcs, J)
solver  = NonlinearVariationalSolver(problem)

prm = solver.parameters
#info(prm, True)
prm["nonlinear_solver"]="newton"   # default.  could be "snes"
prm["newton_solver"]["absolute_tolerance"]   = 1E-8
prm["newton_solver"]["relative_tolerance"]   = 1E-7
prm["newton_solver"]["maximum_iterations"]   =  25 
prm["newton_solver"]["relaxation_parameter"] = 1.0
iterative_solver = False
if iterative_solver:
    prm["newton_solver"]["linear_solver"] = "gmres"
    prm["newton_solver"]["krylov_solver"]["absolute_tolerance"] = 1E-9
    prm["newton_solver"]["krylov_solver"]["relative_tolerance"] = 1E-7
    prm["newton_solver"]["krylov_solver"]["maximum_iterations"] = 1000
    prm["newton_solver"]["krylov_solver"]["monitor_convergence"] = True
    prm["newton_solver"]["krylov_solver"]["nonzero_initial_guess"] = False
    prm["newton_solver"]["krylov_solver"]["gmres"]["restart"] = 40
    prm["newton_solver"]["preconditioner"] = "jacobi"
    prm["newton_solver"]["krylov_solver"]["preconditioner"]["structure"] = "same_nonzero_pattern"
    prm["newton_solver"]["krylov_solver"]["preconditioner"]["ilu"]["fill_level"] = 0

#set_log_level(PROGRESS)

solver.solve()

# Find max error
u_exact = Expression("pow((pow(2, m+1)-1)*x[0] + 1, 1.0/(m+1)) - 1", m=m)
u_e = interpolate(u_exact, V)
import numpy
diff = numpy.abs(u_e.vector().array() - u_.vector().array()).max()
print "Max error:", diff
