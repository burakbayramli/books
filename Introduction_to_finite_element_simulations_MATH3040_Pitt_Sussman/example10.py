"""
example10.py
From FEniCS tutorial demo program
fenicsCh1Tutorial/stationary/nonlinear_poisson/picard_np.py

1D Nonlinear Poisson equation with Dirichlet conditions
at x=0 and x=1.

-div(q(u)*nabla_grad(u)) = 0,
u = 0 at x=0, u=1 at x=1, 
q(u) = (1+u)^m

Solution method: Picard iteration (successive substitutions).
"""

from dolfin import *
import numpy as np
import scipy.linalg as la

# Create mesh and define function space
mesh = UnitIntervalMesh(10)
V = FunctionSpace(mesh, 'Lagrange', 1)

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

# Define variational problem for Picard iteration
u = TrialFunction(V)
v = TestFunction(V)
u_k = interpolate(Constant(0.0), V)  # previous (known) u
a = inner(q(u_k)*nabla_grad(u), nabla_grad(v))*dx
f = Constant(0.0)
L = f*v*dx

# Picard iterations
u = Function(V)     # new unknown function
eps = 1.0           # error measure ||u-u_k||
rho = 0             # convergence rate
tol = 1.0E-6        # tolerance
iter = 0            # iteration counter
maxiter = 25        # max no of iterations allowed
for iter in range(maxiter):
    solve(a == L, u, bcs)
    diff = u.vector().array() - u_k.vector().array()
    oldeps = eps
    eps = la.norm(diff, ord=np.Inf) / la.norm(u.vector().array(), ord=np.Inf)
    rho = eps/oldeps
    print 'iter=%d: norm=%g, rho=%g' % (iter, eps, rho)
    if eps < tol * (1.0-rho)
        break
    u_k.assign(u)   # update for next iteration

convergence = 'convergence after %d Picard iterations' % iter
if iter >= maxiter:
    convergence = 'no ' + convergence
print convergence

# Find max error
u_exact = Expression('pow((pow(2, m+1)-1)*x[0] + 1, 1.0/(m+1)) - 1', m=m)
u_e = interpolate(u_exact, V)
diff = la.norm((u_e.vector().array() - u.vector().array()), ord=np.Inf)
print 'Max error:', diff
