"""
example6.py, similar to dnr_p2D.py
Poisson equation with Dirichlet, Neumann and Robin conditions.
Same solution as before.
"""

from dolfin import *
import numpy

# Create mesh and define function space
mesh = UnitSquareMesh(30, 20)
V = FunctionSpace(mesh, 'Lagrange', 1)

#-------------- Boundary segment definition step -----------------

# Create mesh function over cell facets
boundary_parts = MeshFunction("size_t", mesh, mesh.topology().dim()-1)

# Mark lower boundary facets as subdomain 0
class LowerRobinBoundary(SubDomain):
    def inside(self, x, on_boundary):
        fuzz = 1E-14   # tolerance for coordinate comparisons
        return on_boundary and abs(x[1]) < fuzz

Gamma_R = LowerRobinBoundary()
Gamma_R.mark(boundary_parts, 0)
q = Expression('1 + x[0]*x[0] + 2*x[1]*x[1]')
p = Constant(100)  # arbitrary function can go here

# Mark upper boundary facets as subdomain 1
class UpperNeumannBoundary(SubDomain):
    def inside(self, x, on_boundary):
        fuzz = 1E-14   # tolerance for coordinate comparisons
        return on_boundary and abs(x[1] - 1) < fuzz

Gamma_N = UpperNeumannBoundary()
Gamma_N.mark(boundary_parts, 1)
g = Expression('-4*x[1]')

# Mark left boundary as subdomain 2
class LeftBoundary(SubDomain):
    def inside(self, x, on_boundary):
        fuzz = 1E-14   # tolerance for coordinate comparisons
        return on_boundary and abs(x[0]) < fuzz

Gamma_0 = LeftBoundary()
Gamma_0.mark(boundary_parts, 2)

# Mark right boundary as subdomain 3
class RightBoundary(SubDomain):
    def inside(self, x, on_boundary):
        fuzz = 1E-14   # tolerance for coordinate comparisons
        return on_boundary and abs(x[0] - 1) < fuzz

Gamma_1 = RightBoundary()
Gamma_1.mark(boundary_parts, 3)

#-------------- Solution and problem definition step -----------------

# Define essential b.c.
u_L = Expression('1 + 2*x[1]*x[1]')
u_R = Expression('2 + 2*x[1]*x[1]')
bcs = [DirichletBC(V, u_L, boundary_parts, 2),
       DirichletBC(V, u_R, boundary_parts, 3)]

# Define variational problem
u = TrialFunction(V)
v = TestFunction(V)
f = Constant(-6.0)
a = inner(nabla_grad(u), nabla_grad(v))*dx + p*u*v*ds(0)
L = f*v*dx - g*v*ds(1) + p*q*v*ds(0)

# Compute matrices
A, b = assemble_system(a, L, bcs, exterior_facet_domains=boundary_parts)

u = Function(V)
solve(A, u.vector(), b, 'lu')

# Verification
u_array = u.vector().array()
u_exact = Expression('1 + x[0]*x[0] + 2*x[1]*x[1]')
u_e = interpolate(u_exact, V)
u_e_array = u_e.vector().array()
maxerr = (abs(u_e_array - u_array)).max()
print "max norm error=",maxerr
