"""Lagrange interpolating polynomials in 1D."""
import sympy as sym

def Lagrange_polynomial(x, i, points):
    """
    Return the Lagrange polynomial no. i.
    points are the interpolation points, and x can be a number or
    a sympy.Symbol object (for symbolic representation of the
    polynomial). When x is a sympy.Symbol object, it is
    normally desirable (for nice output of polynomial expressions)
    to let points consist of integers or rational numbers in sympy.
    """
    p = 1
    for k in range(len(points)):
        if k != i:
            p *= (x - points[k])/(points[i] - points[k])
    return p

def Lagrange_polynomials_01(x, N):
    """
    Compute all the Lagrange polynomials (at x) for N+1 equally
    spaced interpolation points on [0,1]. The polynomials
    and points, as two separate lists, are returned.
    Works for symbolic and numerical computation of the
    polynomials and points (if x is sympy.Symbol, symbolic
    expressions are made with rational expressions for the
    points, otherwise floating-point numbers are used).
    """
    if isinstance(x, sym.Symbol):
        h = sym.Rational(1, N)
    else:
        h = 1.0/N
    points = [i*h for i in range(N+1)]
    psi = [Lagrange_polynomial(x, i, points) for i in range(N+1)]
    return psi, points

def Chebyshev_nodes(a, b, N):
    """Return N+1 Chebyshev nodes (for interpolation) on [a, b]."""
    from math import cos, pi
    half = 0.5
    nodes = [0.5*(a+b) + 0.5*(b-a)*cos(float(2*i+1)/(2*(N+1))*pi)
             for i in range(N+1)]
    return nodes

def Lagrange_polynomials(x, N, Omega, point_distribution='uniform'):
    """
    Compute all the Lagrange polynomials (at x) on an interval
    Omega. N is the degree of the polynomials.
    The points are distributed uniformly if point_distribution='uniform',
    if the value is 'Chebyshev' the Chebyshev nodes are used.
    If x is sympy.Symbol, rational expressions (in sympy) are used
    for the points if they are distributed uniformly. Otherwise, the
    points are floating-point numbers. In this way, the function
    works for both symbolic and numeric expressions for the
    Lagrange polynomials.
    """
    if point_distribution == 'uniform':
        if isinstance(x, sym.Symbol):
            h = sym.Rational(Omega[1] - Omega[0], N)
        else:
            h = (Omega[1] - Omega[0])/float(N)  # float value
        points = [Omega[0] + i*h for i in range(N+1)]
    elif point_distribution == 'Chebyshev':
        points = Chebyshev_nodes(Omega[0], Omega[1], N)
    else:
        raise ValueError('point_distribution="%s": illegal value' %
                         point_distribution)
    psi = [Lagrange_polynomial(x, i, points) for i in range(N+1)]
    return psi, points
