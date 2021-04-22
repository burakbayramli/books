"""
Example3.py
3D Poisson equation with Dirichlet conditions.

-Laplace(u) = f on the unit square.
u = u0 on the boundary.
u0 = u = 1 + x^2 + 2y^2 + 3z^2, f = -12.
"""

from dolfin import *

set_log_level(DEBUG)  # show some of what is going on

# Create mesh and define function space
mesh = UnitCubeMesh(50, 50, 50)
V = FunctionSpace(mesh, 'Lagrange', 2)

# Define boundary conditions
u0 = Expression('1 + x[0]*x[0] + 2*x[1]*x[1] + 3*x[2]*x[2]')

def u0_boundary(x, on_boundary):
    return on_boundary

bc = DirichletBC(V, u0, u0_boundary)

# Define variational problem
u = TrialFunction(V)
v = TestFunction(V)
f = Constant(-6.0)
a = inner(nabla_grad(u), nabla_grad(v))*dx
L = f*v*dx

# Compute solution
u = Function(V)
solve(a == L, u, bc, solver_parameters=dict(linear_solver="cg",
                                        preconditioner="bjacobi"))

if False:
    # Plot solution and mesh
    plot(u)
    plot(mesh)
    # Hold plot
    interactive()
