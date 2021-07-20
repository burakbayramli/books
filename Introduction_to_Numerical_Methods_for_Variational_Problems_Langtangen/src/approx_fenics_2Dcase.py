"""
Special 2D approximation case using FEniCS.
An even more general code is found in approx_fenics.py.
"""
from fenics import *

def approx(f, V):
     """Return Galerkin approximation to f in V."""
     u = TrialFunction(V)
     v = TestFunction(V)
     a = u*v*dx
     L = f*v*dx
     u = Function(V)
     solve(a == L, u)
     return u

def problem():
    f = Expression('2*x[0]*x[1] - pow(x[0], 2)', degree=2)
    mesh = RectangleMesh(Point(0,-1), Point(2,1), 8, 8)

    V1 = FunctionSpace(mesh, 'P', 1)
    u1 = approx(f, V1)
    u1.rename('u1', 'u1')
    u1_error = errornorm(f, u1, 'L2')
    u1_norm = norm(u1, 'L2')

    V2 = FunctionSpace(mesh, 'P', 2)
    u2 = approx(f, V2)
    u2.rename('u2', 'u2')
    u2_error = errornorm(f, u2, 'L2')
    u2_norm = norm(u2, 'L2')

    print('L2 errors: e1=%g, e2=%g' % (u1_error, u2_error))
    print('L2 norms:  n1=%g, n2=%g' % (u1_norm, u2_norm))
    # Simple plotting
    import matplotlib.pyplot as plt
    plot(f, title='f', mesh=mesh)
    plt.show()
    plot(u1, title='u1')
    plt.show()
    plot(u2, title='u2')
    plt.show()

if __name__ == '__main__':
    problem()
