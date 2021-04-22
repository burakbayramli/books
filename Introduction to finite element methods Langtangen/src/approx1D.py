"""
Approximation of functions by linear combination of basis functions in
function spaces and the least squares method or the collocation method
for determining the coefficients.
"""

from __future__ import print_function

import sympy as sym
import numpy as np
import mpmath
import matplotlib.pyplot as plt
#import scitools.std as plt


def least_squares_non_verbose(f, psi, Omega, symbolic=True):
    """
    Given a function f(x) on an interval Omega (2-list)
    return the best approximation to f(x) in the space V
    spanned by the functions in the list psi.
    """
    N = len(psi) - 1
    A = sym.zeros(N+1, N+1)
    b = sym.zeros(N+1, 1)
    x = sym.Symbol('x')
    for i in range(N+1):
        for j in range(i, N+1):
            integrand = psi[i]*psi[j]
            integrand = sym.lambdify([x], integrand, 'mpmath')
            I = mpmath.quad(integrand, [Omega[0], Omega[1]])
            A[i,j] = A[j,i] = I
        integrand = psi[i]*f
        integrand = sym.lambdify([x], integrand, 'mpmath')
        I = mpmath.quad(integrand, [Omega[0], Omega[1]])
        b[i,0] = I
    c = mpmath.lu_solve(A, b)  # numerical solve
    c = [c[i,0] for i in range(c.rows)]

    u = sum(c[i]*psi[i] for i in range(len(psi)))
    return u, c



def least_squares(f, psi, Omega, symbolic=True):
    """
    Given a function f(x) on an interval Omega (2-list)
    return the best approximation to f(x) in the space V
    spanned by the functions in the list psi.
    """
    N = len(psi) - 1
    A = sym.zeros(N+1, N+1)
    b = sym.zeros(N+1, 1)
    x = sym.Symbol('x')
    print('...evaluating matrix...', end=' ')
    for i in range(N+1):
        for j in range(i, N+1):
            print('(%d,%d)' % (i, j))

            integrand = psi[i]*psi[j]
            if symbolic:
                I = sym.integrate(integrand, (x, Omega[0], Omega[1]))
            if not symbolic or isinstance(I, sym.Integral):
                # Could not integrate symbolically, use numerical int.
                print('numerical integration of', integrand)
                integrand = sym.lambdify([x], integrand, 'mpmath')
                I = mpmath.quad(integrand, [Omega[0], Omega[1]])
            A[i,j] = A[j,i] = I
        integrand = psi[i]*f
        if symbolic:
            I = sym.integrate(integrand, (x, Omega[0], Omega[1]))
        if not symbolic or isinstance(I, sym.Integral):
            # Could not integrate symbolically, use numerical int.
            print('numerical integration of', integrand)
            integrand = sym.lambdify([x], integrand, 'mpmath')
            I = mpmath.quad(integrand, [Omega[0], Omega[1]])
        b[i,0] = I
    print()
    print('A:\n', A, '\nb:\n', b)
    if symbolic:
        c = A.LUsolve(b)  # symbolic solve
        # c is a sympy Matrix object, numbers are in c[i,0]
        c = [sym.simplify(c[i,0]) for i in range(c.shape[0])]
    else:
        c = mpmath.lu_solve(A, b)  # numerical solve
        c = [c[i,0] for i in range(c.rows)]
    print('coeff:', c)

    u = sum(c[i]*psi[i] for i in range(len(psi)))
    print('approximation:', u)
    return u, c

def numerical_linsys_solve(A, b, floating_point_calc='sympy'):
    """
    Given a linear system Au=b as sympy arrays, solve the
    system using different floating-point software.
    floating_point_calc may be 'sympy', 'numpy.float64',
    'numpy.float32'.
    This function is used to investigate ill-conditioning
    of linear systems arising from approximation methods.
    """
    if floating_point_calc == 'sympy':
        #mpmath.mp.dsp = 10  # does not affect the computations here
        A = mpmath.fp.matrix(A)
        b = mpmath.fp.matrix(b)
        print('A:\n', A, '\nb:\n', b)
        c = mpmath.fp.lu_solve(A, b)
        #c = mpmath.lu_solve(A, b) # more accurate
        print('mpmath.fp.lu_solve:', c)
    elif floating_point_calc.startswith('numpy'):
        import numpy as np
        # Double precision (float64) by default
        A = np.array(A.evalf())
        b = np.array(b.evalf())
        if floating_point_calc == 'numpy.float32':
            # Single precision
            A = A.astype(np.float32)
            b = b.astype(np.float32)
        c = np.linalg.solve(A, b)
        print('numpy.linalg.solve, %s:' % floating_point_calc, c)


def least_squares_orth(f, psi, Omega, symbolic=True):
    """
    Same as least_squares, but for orthogonal
    basis such that one avoids calling up standard
    Gaussian elimination.
    """
    N = len(psi) - 1
    A = [0]*(N+1)       # plain list to hold symbolic expressions
    b = [0]*(N+1)
    x = sym.Symbol('x')
    print('...evaluating matrix...', end=' ')
    for i in range(N+1):
        print('(%d,%d)' % (i, i))
        # Assume orthogonal psi can be be integrated symbolically
        # and that this is a successful/possible integration
        A[i] = sym.integrate(psi[i]**2, (x, Omega[0], Omega[1]))

        # Fallback on numerical integration if f*psi is too difficult
        # to integrate
        integrand = psi[i]*f
        if symbolic:
            I = sym.integrate(integrand,  (x, Omega[0], Omega[1]))
        if not symbolic or isinstance(I, sym.Integral):
            print('numerical integration of', integrand)
            integrand = sym.lambdify([x], integrand, 'mpmath')
            I = mpmath.quad(integrand, [Omega[0], Omega[1]])
        b[i] = I
    print('A:\n', A, '\nb:\n', b)
    c = [b[i]/A[i] for i in range(len(b))]
    print('coeff:', c)
    u = 0
    #for i in range(len(psi)):
    #    u += c[i]*psi[i]
    u = sum(c[i]*psi[i] for i in range(len(psi)))
    print('approximation:', u)
    return u, c

def trapezoidal(values, dx):
    """
    Integrate a function whose values on a mesh with spacing dx
    are in the array values.
    """
    #return dx*np.sum(values)
    return dx*(np.sum(values) - 0.5*values[0] - 0.5*values[-1])


def least_squares_numerical(f, psi, N, x,
                            integration_method='scipy',
                            orthogonal_basis=False):
    """
    Given a function f(x) (Python function), a basis specified by the
    Python function psi(x, i), and a mesh x (array), return the best
    approximation to f(x) in in the space V spanned by the functions
    in the list psi. The best approximation is represented as an array
    of values corresponding to x.  All calculations are performed
    numerically. integration_method can be `scipy` or `trapezoidal`
    (the latter uses x as mesh for evaluating f).
    """
    import scipy.integrate
    A = np.zeros((N+1, N+1))
    b = np.zeros(N+1)
    if not callable(f) or not callable(psi):
        raise TypeError('f and psi must be callable Python functions')
    Omega = [x[0], x[-1]]
    dx = x[1] - x[0]       # assume uniform partition

    print('...evaluating matrix...', end=' ')
    for i in range(N+1):
        j_limit = i+1 if orthogonal_basis else N+1
        for j in range(i, j_limit):
            print('(%d,%d)' % (i, j))
            if integration_method == 'scipy':
                A_ij = scipy.integrate.quad(
                    lambda x: psi(x,i)*psi(x,j),
                    Omega[0], Omega[1], epsabs=1E-9, epsrel=1E-9)[0]
            elif integration_method == 'sympy':
                A_ij = mpmath.quad(
                    lambda x: psi(x,i)*psi(x,j),
                    [Omega[0], Omega[1]])
            else:
                values = psi(x,i)*psi(x,j)
                A_ij = trapezoidal(values, dx)
            A[i,j] = A[j,i] = A_ij

        if integration_method == 'scipy':
            b_i = scipy.integrate.quad(
                lambda x: f(x)*psi(x,i), Omega[0], Omega[1],
                epsabs=1E-9, epsrel=1E-9)[0]
        elif integration_method == 'sympy':
            b_i = mpmath.quad(
                lambda x: f(x)*psi(x,i), [Omega[0], Omega[1]])
        else:
            values = f(x)*psi(x,i)
            b_i = trapezoidal(values, dx)
        b[i] = b_i

    c = b/np.diag(A) if orthogonal_basis else np.linalg.solve(A, b)
    u = sum(c[i]*psi(x, i) for i in range(N+1))
    return u, c


def interpolation(f, psi, points):
    """
    Given a function f(x), return the approximation to
    f(x) in the space V, spanned by psi, that interpolates
    f at the given points. Must have len(points) = len(psi)
    """
    N = len(psi) - 1
    A = sym.zeros(N+1, N+1)
    b = sym.zeros(N+1, 1)
    # Wrap psi and f in Python functions rather than expressions
    # so that we can evaluate psi at points[i] (alternative to subs?)
    psi_sym = psi  # save symbolic expression
    x = sym.Symbol('x')
    psi = [sym.lambdify([x], psi[i], 'mpmath') for i in range(N+1)]
    f = sym.lambdify([x], f, 'mpmath')
    print('...evaluating matrix...')
    for i in range(N+1):
        for j in range(N+1):
            print('(%d,%d)' % (i, j))
            A[i,j] = psi[j](points[i])
        b[i,0] = f(points[i])
    print()
    print('A:\n', A, '\nb:\n', b)
    c = A.LUsolve(b)
    # c is a sympy Matrix object, turn to list
    c = [sym.simplify(c[i,0]) for i in range(c.shape[0])]
    print('coeff:', c)
#    u = sym.simplify(sum(c[i,0]*psi_sym[i] for i in range(N+1)))
    u = sym.simplify(sum(c[i]*psi_sym[i] for i in range(N+1)))
    print('approximation:', u)
    return u, c

collocation = interpolation  # synonym in this module

def regression(f, psi, points):
    """
    Given a function f(x), return the approximation to
    f(x) in the space V, spanned by psi, using a regression
    method based on points. Must have len(points) > len(psi).
    """
    N = len(psi) - 1
    m = len(points) - 1
    # Use numpy arrays and numerical computing
    B = np.zeros((N+1, N+1))
    d = np.zeros(N+1)
    # Wrap psi and f in Python functions rather than expressions
    # so that we can evaluate psi at points[i]
    x = sym.Symbol('x')
    psi_sym = psi  # save symbolic expression for u
    psi = [sym.lambdify([x], psi[i]) for i in range(N+1)]
    f = sym.lambdify([x], f)
    print('...evaluating matrix...')
    for i in range(N+1):
        for j in range(N+1):
            B[i,j] = 0
            for k in range(m+1):
                B[i,j] += psi[i](points[k])*psi[j](points[k])
        d[i] = 0
        for k in range(m+1):
            d[i] += psi[i](points[k])*f(points[k])
    print('B:\n', B, '\nd:\n', d)
    c = np.linalg.solve(B, d)
    print('coeff:', c)
    u = sum(c[i]*psi_sym[i] for i in range(N+1))
    print('approximation:', sym.simplify(u))
    return u, c

def regression_with_noise(f, psi, points):
    """
    Given a data points in the array f, return the approximation
    to the data in the space V, spanned by psi, using a regression
    method based on f and the corresponding coordinates in points.
    Must have len(points) = len(f) > len(psi).
    """
    N = len(psi) - 1
    m = len(points) - 1
    # Use numpy arrays and numerical computing
    B = np.zeros((N+1, N+1))
    d = np.zeros(N+1)
    # Wrap psi and f in Python functions rather than expressions
    # so that we can evaluate psi at points[i]
    x = sym.Symbol('x')
    psi_sym = psi  # save symbolic expression for u
    psi = [sym.lambdify([x], psi[i]) for i in range(N+1)]
    if not isinstance(f, np.ndarray):
        raise TypeError('f is %s, must be ndarray' % type(f))
    print('...evaluating matrix...')
    for i in range(N+1):
        for j in range(N+1):
            B[i,j] = 0
            for k in range(m+1):
                B[i,j] += psi[i](points[k])*psi[j](points[k])
        d[i] = 0
        for k in range(m+1):
            d[i] += psi[i](points[k])*f[k]
    print('B:\n', B, '\nd:\n', d)
    c = np.linalg.solve(B, d)
    print('coeff:', c)
    u = sum(c[i]*psi_sym[i] for i in range(N+1))
    print('approximation:', sym.simplify(u))
    return u, c

def comparison_plot(
    f, u, Omega, filename='tmp',
    plot_title='', ymin=None, ymax=None,
    u_legend='approximation',
    points=None, point_values=None, points_legend=None,
    legend_loc='upper right',
    show=True):
    """Compare f(x) and u(x) for x in Omega in a plot."""
    x = sym.Symbol('x')
    print('f:', f)
    print('u:', u)

    f = sym.lambdify([x], f, modules="numpy")
    u = sym.lambdify([x], u, modules="numpy")
    if len(Omega) != 2:
        raise ValueError('Omega=%s must be an interval (2-list)' % str(Omega))
    # When doing symbolics, Omega can easily contain symbolic expressions,
    # assume .evalf() will work in that case to obtain numerical
    # expressions, which then must be converted to float before calling
    # linspace below
    if not isinstance(Omega[0], (int,float)):
        Omega[0] = float(Omega[0].evalf())
    if not isinstance(Omega[1], (int,float)):
        Omega[1] = float(Omega[1].evalf())

    resolution = 601  # no of points in plot (high resolution)
    xcoor = np.linspace(Omega[0], Omega[1], resolution)
    # Vectorized functions expressions does not work with
    # lambdify'ed functions without the modules="numpy"
    exact  = f(xcoor)
    approx = u(xcoor)
    plt.figure()
    plt.plot(xcoor, approx, '-')
    plt.plot(xcoor, exact, '--')
    legends = [u_legend, 'exact']
    if points is not None:
        if point_values is None:
            # Use f
            plt.plot(points, f(points), 'ko')
        else:
            # Use supplied points
            plt.plot(points, point_values, 'ko')
        if points_legend is not None:
            legends.append(points_legend)
        else:
            legends.append('points')
    plt.legend(legends, loc=legend_loc)
    plt.title(plot_title)
    plt.xlabel('x')
    if ymin is not None and ymax is not None:
        plt.axis([xcoor[0], xcoor[-1], ymin, ymax])
    plt.savefig(filename + '.pdf')
    plt.savefig(filename + '.png')
    if show:
        plt.show()

if __name__ == '__main__':
    print('Module file not meant for execution.')
