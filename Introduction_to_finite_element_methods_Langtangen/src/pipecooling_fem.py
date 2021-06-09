def boundary(x):
  return x[0] < DOLFIN_EPS or x[0] > 1.0 - DOLFIN_EPS

from dolfin import *
import matplotlib.pyplot as plt

Ns = [2, 4, 8, 16, 32]
for N in Ns: 
    mesh = UnitIntervalMesh(N)
    V = FunctionSpace(mesh, "Lagrange", 1)
    u = TrialFunction(V)
    v = TestFunction(V) 

    beta = Constant(1)
    mu = Constant(1) 

    bc = DirichletBC(V, Constant(0), boundary)
    a = mu*inner(grad(u), grad(v))*dx 
    L = -beta*v*dx 

    w = Function(V)
    solve(a == L, w, bc)

    plt.plot(V.tabulate_dof_coordinates(), w.vector().get_local())
    plt.hold(True)
plt.legend(["N=%d"%N for N in Ns], loc="upper left")
plt.show()



for N in Ns: 
    mesh = UnitIntervalMesh(N)
    V = FunctionSpace(mesh, "Lagrange", 1)
    u = TrialFunction(V)
    v = TestFunction(V) 

    beta = Constant(1)
    mu = Constant(1) 

    bc = DirichletBC(V, Constant(0), boundary)
    a = mu*inner(grad(u), grad(v))*dx 
    L = -beta*v*dx 

    w = Function(V)
    solve(a == L, w, bc)


    T0 = Constant(1)
    kappa = Constant(1)
    bc = DirichletBC(V, T0, boundary)
    a = kappa*inner(grad(u), grad(v))*dx 
    L = -mu*inner(grad(w), grad(w))*v*dx 

    T = Function(V)
    solve(a == L, T, bc)

    plt.plot(V.tabulate_dof_coordinates(), T.vector().get_local())
    plt.hold(True)
plt.legend(["N=%d"%N for N in Ns], loc="upper left")
plt.savefig('fenics_cooling.png'); plt.savefig('fenics_cooling.pdf')
plt.show()






#  plot(w)
#  plot(T)
#  interactive()



