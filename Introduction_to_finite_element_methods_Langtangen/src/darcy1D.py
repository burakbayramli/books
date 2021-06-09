import matplotlib.pyplot as plt
from fenics import *

class uExact(UserExpression):
    def __init__(self, **kwargs):
        super().__init__(degree=kwargs["degree"])
        self.a0 = kwargs["a0"]
        self.a  = 1
    def eval(self, value, x):
        if x[0] < 0.5:
            value[0] = (2.0*self.a0 / (self.a0 +1)) / self.a * x[0]
        else:
            value[0] = ((2.0*self.a0 / (self.a0 +1)) / self.a0) * x[0] \
                       + (self.a0-1)/(self.a0+1)

class A(UserExpression):
    def __init__(self, **kwargs):
        super().__init__(degree=kwargs["degree"])
        self.a0 = kwargs["a0"]
    def eval(self, value, x):
        value[0] = 1
        if x[0] >= 0.5: value[0] = self.a0

class DirichletBoundary(SubDomain):
    def inside(self, x, on_boundary):
        return on_boundary

p_bc = f = Expression("x[0]", degree=2)
Ns = [2, 8, 32]
a0 = 0.1
for N in Ns:
    mesh = UnitIntervalMesh(N)
    V = FunctionSpace(mesh, "CG", 1)
    Q = FunctionSpace(mesh, "DG", 0)
    u = TrialFunction(V)
    v = TestFunction(V)
    a_coeff = A(degree=2, a0=a0)
    a = a_coeff*inner(grad(u), grad(v))*dx
    f = Constant(0)
    L = f*v*dx
    bc = DirichletBC(V, p_bc, DirichletBoundary())
    u = Function(V)
    solve(a == L, u, bc)

    # plot solution on the various meshes
    plt.plot(V.tabulate_dof_coordinates(), u.vector().get_local())

# create plot for analytical solution, plot, save
u_exact = project(uExact(a0=a0, degree=1), V)
plt.plot(V.tabulate_dof_coordinates(), u_exact.vector().get_local())
legend = ["N=%d"%N for N in Ns]
legend.append("analytical solution")
plt.legend(legend, loc="upper left")
plt.savefig('darcy_a1D.png'); plt.savefig('darcy_a1D.pdf')
plt.show()

for N in Ns:
    mesh = UnitIntervalMesh(N)
    V = FunctionSpace(mesh, "CG", 1)
    u = TrialFunction(V)
    v = TestFunction(V)
    a_coeff = A(degree=2, a0=a0)
    a = a_coeff*inner(grad(u), grad(v))*dx
    f = Constant(0)
    L = f*v*dx
    bc = DirichletBC(V, p_bc, DirichletBoundary())
    u = Function(V)
    solve(a == L, u, bc)
    aux = project(-a_coeff*u.dx(0), V)

    plt.ylim([-0.4,-0.1])
    plt.plot(V.tabulate_dof_coordinates(), aux.vector().get_local())
    plt.legend(["N=%d"%N for N in Ns], loc="upper left")
plt.savefig('darcy_adx1D.png'); plt.savefig('darcy_adx1D.pdf')
plt.show()
