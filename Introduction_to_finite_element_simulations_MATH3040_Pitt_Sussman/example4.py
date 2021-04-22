"""
example4.py: Poisson equation with Dirichlet conditions.
With computations and visualizations of grad(u).

-Laplace(u) = f on the unit square.
u = u0 on the boundary.
u0 = u = xy, f = 0.
"""

from dolfin import *
import numpy as np

# Create mesh and define function space
mesh = UnitSquareMesh(20, 20)
V = FunctionSpace(mesh, "Lagrange", 1)


# Define boundary conditions
u0=Expression("x[0]*x[1]")
def u0_boundary(x, on_boundary):
    return on_boundary

bc = DirichletBC(V, u0, u0_boundary)

# Define variational problem
u = TrialFunction(V)
v = TestFunction(V)
f = Constant(-0.0)
a = inner(grad(u), grad(v))*dx
L = f*v*dx

# Compute solution
u = Function(V)
solve(a == L, u, bc)
u.rename("u", "solution field")
u_array = u.vector().array()    # dangerous in parallel context

# Compute gradient
V_g = VectorFunctionSpace(mesh, "Lagrange", 1)
v = TestFunction(V_g)
w = TrialFunction(V_g)

a = inner(w, v)*dx
L = inner(grad(u), v)*dx
grad_u = Function(V_g)
solve(a == L, grad_u)
grad_u.rename("grad(u)", "continuous gradient field")

#plot(u, title=u.name())
plot(grad_u, title=grad_u.name())

grad_u_x, grad_u_y = grad_u.split(deepcopy=True)  # extract components
grad_u_x.rename("grad(u)_x", "x-component of grad(u)")
grad_u_y.rename("grad(u)_y", "y-component of grad(u)")
#plot(grad_u_x, title=grad_u_x.label())
#plot(grad_u_y, title=grad_u_y.label())

# Alternative computation of grad(u)
grad_u2 = project(grad(u), VectorFunctionSpace(mesh, "Lagrange", 1))
grad_u2_x, grad_u2_y = grad_u2.split(deepcopy=True)

plot(grad_u2, title="plot projected grad")

# Verification
u_e = interpolate(u0, V)
u_e_array = u_e.vector().array()
print "Max error:", np.abs(u_e_array - u_array).max()

# exact expressions for grad u
u1 = Expression("x[1]")
u2 = Expression("x[0]")
grad_u_e_x=interpolate(u1,V)
grad_u_e_y=interpolate(u2,V)

u_error = (u - u_e)**2*dx
grad_error =  (grad_u_x -grad_u_e_x)**2*dx + (grad_u_y -grad_u_e_y)**2*dx
grad2_error = (grad_u2_x-grad_u_e_x)**2*dx + (grad_u2_y-grad_u_e_y)**2*dx
u_norm = u**2*dx
grad_norm = grad_u_e_x**2*dx + grad_u_e_y**2*dx
Err_u  = sqrt(abs(assemble(u_error    )))
Err_g  = sqrt(abs(assemble(grad_error )))
Err_g2 = sqrt(abs(assemble(grad2_error)))

Nrm_u  = sqrt(abs(assemble(u_norm     )))
Nrm_g  = sqrt(abs(assemble(grad_norm  )))
print "Err_u=",Err_u/Nrm_u
print "Err_g=",Err_g/Nrm_g
print "Err_g2=",Err_g2/Nrm_g

interactive()
