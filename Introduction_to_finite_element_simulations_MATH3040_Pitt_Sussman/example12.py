"""
example12.py,
Nonlinear 1D Poisson equation with Dirichlet conditions

-div(q(u)*nabla_grad(u)) = 0,
u = 0 at x=0, u=1 at x=1, q(u) = (1+u)^m

Solution method: Newton iteration, automatic UFL computation of J
"""

from dolfin import *
import numpy as np
import scipy.linalg as la

# Create mesh and define function space
mesh = UnitIntervalMesh(20)
V = FunctionSpace(mesh, 'Lagrange', 1)

# Define boundary conditions for initial guess
fuzz = 1E-14
def left_boundary(x, on_boundary):
    return on_boundary and abs(x[0]) < fuzz

def right_boundary(x, on_boundary):
    return on_boundary and abs(x[0]-1) < fuzz

Gamma_0 = DirichletBC(V, Constant(0.0), left_boundary)
Gamma_1 = DirichletBC(V, Constant(1.0), right_boundary)
bcs = [Gamma_0, Gamma_1]

# Define variational problem for initial guess (q(u)=1, i.e., m=0)
u = TrialFunction(V)
v = TestFunction(V)
a = inner(nabla_grad(u), nabla_grad(v))*dx
f = Constant(0.0)
L = f*v*dx
A, b = assemble_system(a, L, bcs)
u_k = Function(V)
solve(A, u_k.vector(), b, 'lu')

# Note that all Dirichlet conditions must be zero for
# the correction function in a Newton-type method
Gamma_0_du = DirichletBC(V, Constant(0.0), left_boundary)
Gamma_1_du = DirichletBC(V, Constant(0.0), right_boundary)
bcs_du = [Gamma_0_du, Gamma_1_du]

# Choice of nonlinear coefficient
m = 2
def q(u):
    return (1+u)**m

# Define variational problem for the matrix and vector
# in a Newton iteration
du = TrialFunction(V) # u = u_k + omega*du
L = inner(q(u_k)*nabla_grad(u_k), nabla_grad(v))*dx
J = derivative(L, u_k, du)
L = -L

# Newton iteration at the algebraic level
du = Function(V)
u  = Function(V)  # u = u_k + omega*du
omega = 1.0       # relaxation parameter
err = 1.0
tol = 1.0E-5
iter = 0
maxiter = 25
# u_k has correct nonhomogeneous boundary conditions
u.assign(u_k)
for iter in range(maxiter):
    print iter, "iteration",
    AJ, b = assemble_system(J, L, bcs_du)
    solve(AJ, du.vector(), b)
    u.vector()[:] += omega*du.vector()
    olderr = err
    err = la.norm(du.vector().array(), ord=np.Inf) / \
          la.norm(u.vector().array(), ord=np.Inf)
    rho = err/olderr
    print "Norm=%g, rho=%g"% (err, rho)
    if err < tol * (1.0 - rho):
        break
    u_k.assign(u)

convergence = "convergence after %d Newton iterations" % iter
if iter >= maxiter:
    convergence = "no " + convergence

# Find max error
u_exact = Expression("pow((pow(2, m+1)-1)*x[0] + 1, 1.0/(m+1)) - 1", m=m)
u_e = interpolate(u_exact, V)
diff = la.norm((u_e.vector().array() - u.vector().array()), ord=np.Inf)
print "Max error:", diff
