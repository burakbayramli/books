"""
example14.py, from d1_d2D.py
FEniCS tutorial demo program: Diffusion equation with Dirichlet
conditions and a solution that will be exact at all nodes.
"""

from dolfin import *
import numpy

# Create mesh and define function space
mesh = UnitSquareMesh(20,10)
V = FunctionSpace(mesh, "Lagrange", 2)

# Define boundary conditions
alpha = 3; beta = 1.2
u0 = Expression("1 + x[0]*x[0] + alpha*x[1]*x[1] + beta*t",
                alpha=alpha, beta=beta, t=0)

class Boundary(SubDomain):  # define the Dirichlet boundary
    def inside(self, x, on_boundary):
        return on_boundary

boundary = Boundary()
bc = DirichletBC(V, u0, boundary)

# Initial condition
u_k = interpolate(u0, V)
#u_k = project(u0, V)  # will not result in exact solution!

dt = 0.3      # time step

# Define variational problem
u = TrialFunction(V)
v = TestFunction(V)
f = Constant(beta - 2 - 2*alpha)
a = u*v*dx + dt*inner(nabla_grad(u), nabla_grad(v))*dx
L = (u_k + dt*f)*v*dx

A = assemble(a)   # assemble only once, before the time stepping
b = None          # trick: first time through loop below, assemble creates b

# Timestep loop
u = Function(V)   # the unknown at a new time level
T = 1.9           # total simulation time
t = dt
while t <= T:
    print "time =", t ,
    b = assemble(L, tensor=b)
    u0.t = t
    bc.apply(A, b)
    solve(A, u.vector(), b)

    # Verify
    u_e = interpolate(u0, V)
    diff = numpy.abs(u_e.vector().array() - u.vector().array())
    print "Max error: %-10.3e" %  diff.max()

    t += dt
    u_k.assign(u)

