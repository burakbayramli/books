import numpy as np
import matplotlib.pyplot as plt

def comparison_plot2D(
    u, f,           # Function expressions in x and y
    value=0.5,      # x or y equals this value
    variation='y',  # independent variable
    n=100,          # no of intervals in plot
    tol=1E-8,       # tolerance for points inside the domain
    plottitle='',   # heading in plot
    filename='tmp', # stem of filename
    ):
    """
    Plot u and f along a line in x or y dir with n intervals
    and a tolerance of tol for points inside the domain.
    """
    v = np.linspace(-1+tol, 1-tol, n+1)
    # Compute points along specified line:
    points = np.array([(value, v_)
                       if variation == 'y' else (v_, value)
                       for v_ in v])
    u_values = [u(point) for point in points] # eval. Function
    f_values = [f(point) for point in points]
    plt.figure()
    plt.plot(v, u_values, 'r-', v, f_values, 'b--')
    plt.legend(['u', 'f'], loc='upper left')
    if variation == 'y':
        plt.xlabel('y'); plt.ylabel('u, f')
    else:
        plt.xlabel('x'); plt.ylabel('u, f')
    plt.title(plottitle)
    plt.savefig(filename + '.pdf')
    plt.savefig(filename + '.png')

import fenics as fe
import sympy as sym
x, y = sym.symbols('x[0] x[1]')

def problem(f, nx=8, ny=8, degrees=[1,2]):
    """
    Plot u along x=const or y=const for Lagrange elements,
    of given degrees, on a nx times ny mesh. f is a SymPy expression.
    """
    f = sym.printing.ccode(f)
    f = fe.Expression(f, degree=2)
    mesh = fe.RectangleMesh(
        fe.Point(-1, 0), fe.Point(1, 2), 2, 2)
    for degree in degrees:
        if degree == 0:
            # The P0 element is specified like this in FEniCS
            V = fe.FunctionSpace(mesh, 'DG', 0)
        else:
            # The Lagrange Pd family of elements, d=1,2,3,...
            V = fe.FunctionSpace(mesh, 'P', degree)
        u = fe.project(f, V)
        u_error = fe.errornorm(f, u, 'L2')
        print('||u-f||=%g' % u_error, degree)
        comparison_plot2D(
            u, f,
            n=50,
            value=0.4, variation='x',
            plottitle='Approximation by P%d elements' % degree,
            filename='approx_fenics_by_P%d' % degree,
            tol=1E-3)
        #fe.plot(u, title='Approx by P%d' % degree)

if __name__ == '__main__':
    # x and y are global SymPy variables
    f = 2*x*y - x**16
    f = 2*x*y - x**2
    problem(f, nx=2, ny=2, degrees=[0, 1, 2])
    plt.show()
