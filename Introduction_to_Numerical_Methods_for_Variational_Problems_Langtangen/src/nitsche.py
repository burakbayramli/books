import matplotlib.pyplot as plt
from dolfin import * 

mesh = UnitIntervalMesh(100) 
V = FunctionSpace(mesh, "Lagrange", 1) 
u = TrialFunction(V) 
v = TestFunction(V)

lams = [1.001, 1.01, 1.1, 2, 10, 100]
for lam in lams: 
    lam = Constant(lam) 
    h = CellDiameter(mesh)
    n = FacetNormal(mesh)
    f = Expression("-12*pow(x[0], 2)", degree=2) 
    u0 = Expression("pow(x[0],4)", degree=4)

    a = dot(grad(v), grad(u))*dx \
       - dot(grad(v), u*n)*ds \
       - dot(v*n, grad(u))*ds \
       + (lam/h)*v*u*ds
    L = v*f*dx - u0*dot(grad(v), n)*ds + (lam/h)*u0*v*ds

    U = Function(V)
    solve(a == L, U) 

    plt.plot(V.tabulate_dof_coordinates(), U.vector().get_local())

plt.legend(["lam=%4.3f" %lam for lam in lams], loc=2)
plt.show()
