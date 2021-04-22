"""
example5.py: from d6_p2D.py
Poisson equation with Dirichlet conditions,
with a more complicated solution, error computations
and convergence study.
u_e=sin(pi*x)*sin(pi*y), f=2*p**2*u_e (eigenfunction)
"""

from dolfin import *
import sys

def compute(nx, ny, degree):
    # Create mesh and define function space
    mesh = UnitSquareMesh(nx, ny)
    V = FunctionSpace(mesh, 'Lagrange', degree=degree)

    # Define boundary conditions
    def u0_boundary(x, on_boundary):
        return on_boundary
    bc = DirichletBC(V, Constant(0.0), u0_boundary)

    # Exact solution: first eigenfunction
    omega = 1.0
    u_e = Expression('sin(omega*pi*x[0])*sin(omega*pi*x[1])',
                     omega=omega)

    # Define variational problem
    u = TrialFunction(V)
    v = TestFunction(V)
    f = 2*pi**2*omega**2*u_e
    a = inner(nabla_grad(u), nabla_grad(v))*dx
    L = f*v*dx

    # Compute solution
    u = Function(V)
    problem = LinearVariationalProblem(a, L, u, bc)
    solver =  LinearVariationalSolver(problem)
    solver.solve()

    # E1: Function - Expression
    error = (u - u_e)**2*dx
    E1 = sqrt(abs(assemble(error)))

    # E2: Explicit interpolation of u_e onto the same space as u:
    u_e_V = interpolate(u_e, V)
    error = (u - u_e_V)**2*dx
    E2 = sqrt(abs(assemble(error)))

    # Explicit interpolation of u_e to higher-order elements,
    # u will also be interpolated to the space Ve before integration
    Ve = FunctionSpace(mesh, 'Lagrange', degree=5)
    u_e_Ve = interpolate(u_e, Ve)
    error = (u - u_e_Ve)**2*dx
    E3 = sqrt(abs(assemble(error)))

    # Explicit interpolation of u and u_e to higher-order elements,
    u_Ve = interpolate(u, Ve)
    u_e_Ve = interpolate(u_e, Ve)
    e_Ve = Function(Ve)
    e_Ve.vector()[:] = u_e_Ve.vector().array() - u_Ve.vector().array()
    E4 = sqrt(abs(assemble(e_Ve**2*dx, mesh=Ve.mesh())))

    # Infinity norm based on nodal values
    u_e_V = interpolate(u_e, V)
    E5 = abs(u_e_V.vector().array() - u.vector().array()).max()

    # H1 seminorm
    error = inner(grad(e_Ve), grad(e_Ve))*dx
    E6 = sqrt(abs(assemble(error)))

    # Collect error measures in a dictionary with self-explanatory keys
    errors = {'1:u - u_e': E1,
              '2:u - interpolate(u_e,V)': E2,
              '3:interpolate(u,Ve) - interpolate(u_e,Ve)': E3,
              '4:error field': E4,
              '5:infinity norm (of dofs)': E5,
              '6:grad(error field)': E6}

    return errors

# Perform experiments
degree = int(sys.argv[1])
h = []  # element sizes
E = []  # errors
for nx in [4, 8, 16, 32]:
    h.append(1.0/nx)
    E.append(compute(nx, nx, degree))  # list of dicts

# Convergence rates
from math import log as ln  # log is a dolfin name too
error_types = E[0].keys()
for error_type in sorted(error_types):
    print '\nError norm based on', error_type
    for i in range(1, len(E)):
        Ei   = E[i][error_type]  # E is a list of dicts
        Eim1 = E[i-1][error_type]
        r = ln(Ei/Eim1)/ln(h[i]/h[i-1])
        print 'h=%8.2E E=%8.2E r=%.2f' % (h[i], Ei, r)
