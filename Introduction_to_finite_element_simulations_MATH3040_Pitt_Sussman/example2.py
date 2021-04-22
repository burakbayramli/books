"""
example2.py: Similar to FEM1D problem
u''+2u'+u=x+2, Neumann b.c.
"""

from dolfin import *

# Create mesh and define function space
N=10
mesh = UnitIntervalMesh(N)
V = FunctionSpace(mesh, 'Lagrange', 2)

# Define variational problem
u = TrialFunction(V)
v = TestFunction(V)
f = Expression('x[0]+2')
a = (-inner(nabla_grad(u), nabla_grad(v)) \
    + 2*grad(u)[0]*v \
    + u*v)*dx
L = f*v*dx

# Compute solution
u = Function(V)
solve(a == L, u)

if False:
    # Plot solution and mesh
    plot(u)
    # Hold plot
    interactive()

if False:
    # Dump solution to file in VTK format
    file = File('poisson.pvd')
    file << u

# exact for comparison
exact=Expression("(1+x[0])*exp(1-x[0])+x[0]*(1-exp(-x[0]))")

# let's get coordinates, x, at the DOF locations
exF = Expression("x[0]")
exvector = interpolate(exF,V).vector().get_local()

# remember that u and exact are functions
sumsq0=0.
sumsq1=0.
for i in range(exvector.size):
    print "x=", exvector[i], " u=", u(exvector[i]), \
        " uexact=", exact(exvector[i])
    sumsq0+=(u(exvector[i])-exact(exvector[i]))**2
    sumsq1+=u(exvector[i])**2

sumsq0 = sqrt(sumsq0)
sumsq1 = sqrt(sumsq1)
relerr0 = sumsq0/sumsq1

print "N=",N," relative 2-norm error=",relerr0

# alternative computation of error, without coordinates
import scipy.linalg as la
u_array = u.vector().array()
u_e = interpolate(exact, V)
u_e_array = u_e.vector().array()
relerr1 = la.norm(u_e_array - u_array) / la.norm(u_e_array)
print "N=",N," relative 2-norm error=",relerr1
