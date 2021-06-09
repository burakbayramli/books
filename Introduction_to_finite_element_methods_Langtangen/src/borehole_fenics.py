"""Flow around a hole in a porous medium."""
from fenics import *
import numpy as np

def make_mesh(Theta, a, b, nr, nt, s):
    mesh = RectangleMesh(Point(a, 0), Point(b, 1),
                         nr, nt, 'crossed')

    # Define markers for Dirichket boundaries
    tol = 1E-14

    # x=a becomes the inner borehole boundary
    class Inner(SubDomain):
        def inside(self, x, on_boundary):
            return on_boundary and abs(x[0] - a) < tol

    # x=b becomes the outer borehole boundary
    class Outer(SubDomain):
        def inside(self, x, on_boundary):
            return on_boundary and abs(x[0] - b) < tol

    inner = Inner(); outer = Outer();
    markers = MeshFunction('size_t', mesh, mesh.topology().dim() - 1)
    markers.set_all(0)
    inner.mark(markers, 1)
    outer.mark(markers, 2)

    # --- Deform mesh ---

    # First make a denser mesh towards r=a
    x = mesh.coordinates()[:,0]
    y = mesh.coordinates()[:,1]

    def denser(x, y):
        return [a + (b-a)*((x-a)/(b-a))**s, y]

    x_bar, y_bar = denser(x, y)
    xy_bar_coor = np.array([x_bar, y_bar]).transpose()
    mesh.coordinates()[:] = xy_bar_coor

    # Then map onto to a "piece of cake"

    def cylinder(r, s):
        return [r*np.cos(Theta*s), r*np.sin(Theta*s)]

    x_hat, y_hat = cylinder(x_bar, y_bar)
    xy_hat_coor = np.array([x_hat, y_hat]).transpose()
    mesh.coordinates()[:] = xy_hat_coor
    return mesh, markers

def solver(
    mesh,
    markers,  # MeshFunctions for Dirichlet conditions
    alpha,    # Diffusion coefficient
    u_a,      # Inner pressure
    u_b,      # Outer pressure
    degree,   # Element polynomial degree
    filename, # Name of VTK file
    ):
    V = FunctionSpace(mesh, 'P', degree)
    bc_inner = DirichletBC(V, u_a, markers, 1)
    bc_outer = DirichletBC(V, u_b, markers, 2)
    bcs = [bc_inner, bc_outer]

    # Define variational problem
    u = TrialFunction(V)
    v = TestFunction(V)
    a = alpha*dot(grad(u), grad(v))*dx
    L = Constant(0)*v*dx  # L = 0*v*dx = 0 does not work...

    # Compute solution
    u = Function(V)
    solve(a == L, u, bcs)

    f = File("mesh.xml")
    f << mesh

    # Save solution to file in VTK format
    vtkfile = File(filename + '.pvd')
    vtkfile << u

    u.rename('u', 'u'); plot(u); plot(mesh)
    import matplotlib.pyplot as plt
    plt.show()
    return u

def problem():
    mesh, markers = make_mesh(Theta=25*pi/180, a=1, b=2,
                              nr=20, nt=20, s=1.9)
    beta = 5
    solver(mesh, markers, alpha=1, u_a=1, u_b=0, degree=1, filename='tmp')

if __name__ == '__main__':
    problem()
