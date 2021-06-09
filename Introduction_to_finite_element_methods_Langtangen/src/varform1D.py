"""
Solution of 1D differential equation by linear combination of
basis functions in function spaces and a variational formulation
of the differential equation problem.
"""
import sympy as sym
import numpy as np
import mpmath
import matplotlib.pyplot as plt


def solver(integrand_lhs, integrand_rhs, psi, Omega,
           boundary_lhs=None, boundary_rhs=None,
           symbolic=True, verbose=False):
    """
    psi: dictionary of lists, psi[0] holdes the basis functions,
    psi[1] holdes the first-order derivatives, and psi[2] the
    second-order derivatives (and so on), as symbolic expressions.
    integrand_lhs and integrand_rhs are functions of psi
    defining the integrands in integrals over Omega in the variational
    formulation. boundary_lhs/rhs are similar functions defining
    contributions from the boundary (boundary integrals, which are point
    values in 1D).

    if symbolic is False, all integrals are calculated by mpmath.quad
    to high precision.
    if verbose is True, integrations and linear system A*c=b are printed
    during the computations.
    """
    N = len(psi[0]) - 1
    A = sym.zeros(N+1, N+1)
    b = sym.zeros(N+1, 1)
    x = sym.Symbol('x')
    print('...evaluating matrix...', end=' ')
    for i in range(N+1):
        for j in range(i, N+1):
            integrand = integrand_lhs(psi, i, j)
            if verbose:
                print('(%d,%d):' % (i, j), integrand)
            if symbolic:
                I = sym.integrate(integrand, (x, Omega[0], Omega[1]))
                if isinstance(I, sym.Integral):
                    symbolic = False  # force numerical integration hereafter
                    print('numerical integration of', integrand)
            if not symbolic:
                integrand_ = sym.lambdify([x], integrand, 'mpmath')
                try:
                    I = mpmath.quad(integrand_, [Omega[0], Omega[1]])
                except NameError as e:
                    raise NameError('Numerical integration of\n%s\nrequires symbol %s to be given a value' %
                                    (integrand, str(e).split()[2]))
            if boundary_lhs is not None:
                I += boundary_lhs(psi, i, j)
            A[i,j] = A[j,i] = I
        integrand = integrand_rhs(psi, i)
        if verbose:
            print('rhs:', integrand)
        if symbolic:
            I = sym.integrate(integrand, (x, Omega[0], Omega[1]))
            if isinstance(I, sym.Integral):
                symbolic = False
                print('numerical integration of', integrand)
        if not symbolic:
            integrand_ = sym.lambdify([x], integrand, 'mpmath')
            try:
                I = mpmath.quad(integrand_, [Omega[0], Omega[1]])
            except NameError as e:
                raise NameError('Numerical integration of\n%s\nrequires symbol %s to be given a value' %
                                (integrand, str(e).split()[2]))
        if boundary_rhs is not None:
            I += boundary_rhs(psi, i)
        b[i,0] = I
    print()
    if verbose: print('A:\n', A, '\nb:\n', b)
    c = A.LUsolve(b)
    #c = mpmath.lu_solve(A, b)
    c = [c[i,0] for i in range(c.shape[0])]
    if verbose: print('coeff:', c)
    u = 0
    for i in range(len(psi[0])):
        u += c[i]*psi[0][i]
    if verbose: print('approximation:', u)
    return u, c

def collocation(term_lhs, term_rhs, psi, points):
    """
    Solve a differential equation by collocation. term_lhs is
    a function of psi (dict of basis functions and their derivatives)
    and points (the collocation points throughout the domain)
    as well as i and j (the matrix index) returning elements in the
    coefficient matrix, while term_rhs is a function of psi, i and
    points returning the element i in the right-hand side vector.
    Note that the given psi is transformed to Python functions through
    sym.lambdify such that term_lhs and term_rhs can simply evaluate
    psi[0][i], ... at a point.
    """
    N = len(psi[0]) - 1
    A = sym.zeros(N+1, N+1)
    b = sym.zeros(N+1, 1)
    # Wrap psi in Python functions (psi_) rather than expressions
    # so that we can evaluate psi_ at points[i] (alternative to subs?)
    x = sym.Symbol('x')
    psi_ = {}
    module = "numpy" if N > 2 else "sympy"
    for derivative in psi:
        psi_[derivative] = [sym.lambdify([x], psi[derivative][i],
                                        modules="sympy")
                            for i in range(N+1)]
    print('...evaluating matrix...', end=' ')
    for i in range(N+1):
        for j in range(N+1):
            print('(%d,%d)' % (i, j))
            A[i,j] = term_lhs(psi_, points, i, j)
        b[i,0] = term_rhs(psi_, points, i)
    print()

    # Drop symbolic expressions (and symbolic solve) for
    # all but the smallest problems (troubles maybe caused by
    # derivatives of psi that trigger full symbolic expressions
    # in A; this problem is not evident in interpolation in approx1D.py)
    if N > 2:
        A = A.evalf()
        b = b.evalf()
    print('A:\n', A, '\nb:\n', b)
    c = A.LUsolve(b)
    print('coeff:', c)
    u = 0
    for i in range(len(psi_[0])):
        u += c[i,0]*psi_[0][i](x)
    print('approximation:', u)
    return u

def comparison_plot(u, Omega, u_e=None, filename='tmp.eps',
                    plot_title='', ymin=None, ymax=None):
    x = sym.Symbol('x')
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

    resolution = 401  # no of points in plot
    xcoor = np.linspace(Omega[0], Omega[1], resolution)
    # Vectorized functions expressions does not work with
    # lambdify'ed functions without the modules="numpy"
    approx = u(xcoor)
    plt.plot(xcoor, approx)
    legends = ['approximation']
    if u_e is not None:
        exact  = u_e(xcoor)
        plt.plot(xcoor, exact)
        legends = ['exact']
    plt.legend(legends)
    plt.title(plot_title)
    plt.xlabel('x')
    if ymin is not None and ymax is not None:
        plt.axis([xcoor[0], xcoor[-1], ymin, ymax])
    plt.savefig(filename)

if __name__ == '__main__':
    print('Module file not meant for execution.')


