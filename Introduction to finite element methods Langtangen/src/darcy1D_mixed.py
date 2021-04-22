
from fenics import *
import matplotlib.pyplot as plt

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
        if x[0] > 0.5: value[0] = self.a0

p_bc = Expression("x[0]", degree=2)

a0 = 0.1
Ns = [2, 8, 32]
for N in Ns:
    mesh = UnitIntervalMesh(N)
    P1 = FiniteElement("CG", mesh.ufl_cell(), 1)
    P2 = FiniteElement("DG", mesh.ufl_cell(), 0)
    P1xP2 = P1 * P2
    W = FunctionSpace(mesh, P1xP2)
    u, p = TrialFunctions(W)
    v, q = TestFunctions(W)

    f = Constant(0)
    n = FacetNormal(mesh)
    a_coeff = A(degree=1, a0=a0)

    a = (1/a_coeff)*u*v*dx + u.dx(0)*q*dx - v.dx(0)*p*dx
    L = f*q*dx - p_bc*v*n[0]*ds

    up = Function(W)
    solve(a == L, up)

    u, p = up.split()

    import numpy
    a = numpy.array([0.0])
    b = numpy.array([0.0])
    xs = numpy.arange(0.0, 1.0, 0.001)
    ps = numpy.arange(0.0, 1.0, 0.001)
    for i in range(0,len(xs)):
        a[0] = xs[i]
        p.eval(b, a)
        ps[i] = b
    plt.plot(xs, ps)


CG1 = FunctionSpace(mesh, "CG", 1)
p_exact = project(uExact(a0=a0, degree=1), CG1)
p_exact4plot = numpy.array([p_exact(x) for x in xs])
plt.plot(xs, p_exact4plot)
legend = ["N=%d"%N for N in Ns]
legend.append("analytical solution")

plt.legend(legend, loc="upper left")
plt.savefig('darcy_a1D_mx.png'); plt.savefig('darcy_a1D_mx.pdf');
plt.show()


for N in Ns:
    mesh = UnitIntervalMesh(N)
    P1 = FiniteElement("CG", mesh.ufl_cell(), 1)
    P2 = FiniteElement("DG", mesh.ufl_cell(), 0)
    TH = P1 * P2
    W = FunctionSpace(mesh, TH)
    u, p = TrialFunctions(W)
    v, q = TestFunctions(W)

    f = Constant(0)
    n = FacetNormal(mesh)
    a_coeff = A(degree=2, a0=a0)

    a = (1/a_coeff)*u*v*dx + u.dx(0)*q*dx - v.dx(0)*p*dx
    L = f*q*dx - p_bc*v*n[0]*ds

    up = Function(W)
    solve(a == L, up)

    u, p = up.split()

    import numpy
    a = numpy.array([0.0])
    b = numpy.array([0.0])
    xs = numpy.arange(0.0, 1.0, 0.001)
    us = numpy.arange(0.0, 1.0, 0.001)
    for i in range(0,len(xs)):
        a[0] = xs[i]
        u.eval(b, a)
        us[i] = b

    plt.ylim([-0.4,-0.1])
    plt.plot(xs, us)


plt.legend(["N=%d"%N for N in Ns], loc="upper left")
plt.savefig('darcy_adx1D_mx.png'); plt.savefig('darcy_adx1D_mx.pdf');
plt.show()
