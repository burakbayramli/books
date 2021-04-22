"""Purely numerical 1D finite element program."""
import sys, os, time
sys.path.insert(
    0, os.path.join(os.pardir, os.pardir, 'approx', 'src-approx'))
from numint import GaussLegendre, NewtonCotes
from fe_approx1D_numint import u_glob

import sympy as sym
import numpy as np

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

def Chebyshev_nodes(a, b, N):
    """Return N+1 Chebyshev nodes (for interpolation) on [a, b]."""
    from math import cos, pi
    half = 0.5
    nodes = [0.5*(a+b) + 0.5*(b-a)*cos(float(2*i+1)/(2*(N+1))*pi)
             for i in range(N+1)]
    return nodes

def basis(d, point_distribution='uniform', symbolic=False):
    """
    Return all local basis function phi and their derivatives,
    in physical coordinates, as functions of the local point
    X in a 1D element with d+1 nodes.
    If symbolic=True, return symbolic expressions, else
    return Python functions of X.
    point_distribution can be 'uniform' or 'Chebyshev'.

    >>> phi = basis(d=1, symbolic=False)
    >>> phi[0][0](0)  # basis func 0 at X=0
    0.5
    >>> phi[1][0](0, h=0.5)  # 1st x-derivative at X=0
    -2
    """
    X, h = sym.symbols('X h')
    phi_sym = {}
    phi_num = {}
    if d == 0:
        phi_sym[0] = [1]
        phi_sym[1] = [0]
    else:
        if point_distribution == 'uniform':
            nodes = np.linspace(-1, 1, d+1)
        elif point_distribution == 'Chebyshev':
            nodes = Chebyshev_nodes(-1, 1, d)

        phi_sym[0] = [Lagrange_polynomial(X, r, nodes)
                      for r in range(d+1)]
        phi_sym[1] = [sym.simplify(sym.diff(phi_sym[0][r], X)*2/h)
                      for r in range(d+1)]
    # Transform to Python functions
    phi_num[0] = [sym.lambdify([X], phi_sym[0][r])
                  for r in range(d+1)]
    phi_num[1] = [sym.lambdify([X, h], phi_sym[1][r])
                  for r in range(d+1)]
    return phi_sym if symbolic else phi_num

def affine_mapping(X, Omega_e):
    x_L, x_R = Omega_e
    return 0.5*(x_L + x_R) + 0.5*(x_R - x_L)*X

def finite_element1D_naive(
    vertices, cells, dof_map,     # mesh
    essbc,                        # essbc[globdof]=value
    ilhs,
    irhs,
    blhs=lambda e, phi, r, s, X, x, h: 0,
    brhs=lambda e, phi, r, X, x, h: 0,
    intrule='GaussLegendre',
    verbose=False,
    ):
    N_e = len(cells)
    N_n = np.array(dof_map).max() + 1

    A = np.zeros((N_n, N_n))
    b = np.zeros(N_n)

    timing = {}
    t0 = time.clock()

    for e in range(N_e):
        Omega_e = [vertices[cells[e][0]], vertices[cells[e][1]]]
        h = Omega_e[1] - Omega_e[0]

        d = len(dof_map[e]) - 1  # Polynomial degree
        # Compute all element basis functions and their derivatives
        phi = basis(d)

        if verbose:
            print('e=%2d: [%g,%g] h=%g d=%d' % \
                  (e, Omega_e[0], Omega_e[1], h, d))

        # Element matrix and vector
        n = d+1  # No of dofs per element
        A_e = np.zeros((n, n))
        b_e = np.zeros(n)

        # Integrate over the reference cell
        if intrule == 'GaussLegendre':
            points, weights = GaussLegendre(d+1)
        elif intrule == 'NewtonCotes':
            points, weights = NewtonCotes(d+1)

        for X, w in zip(points, weights):
            detJ = h/2
            x = affine_mapping(X, Omega_e)
            dX = detJ*w

            # Compute contribution to element matrix and vector
            for r in range(n):
                for s in range(n):
                    A_e[r,s] += ilhs(e, phi, r, s, X, x, h)*dX
                b_e[r] += irhs(e, phi, r, X, x, h)*dX

        # Add boundary terms
        for r in range(n):
            for s in range(n):
                A_e[r,s] += blhs(e, phi, r, s, X, x, h)
            b_e[r] += brhs(e, phi, r, X, x, h)

        if verbose:
            print('A^(%d):\n' % e, A_e);  print('b^(%d):' % e, b_e)

        # Incorporate essential boundary conditions
        modified = False
        for r in range(n):
            global_dof = dof_map[e][r]
            if global_dof in essbc:
                # dof r is subject to an essential condition
                value = essbc[global_dof]
                # Symmetric modification
                b_e -= value*A_e[:,r]
                A_e[r,:] = 0
                A_e[:,r] = 0
                A_e[r,r] = 1
                b_e[r] = value
                modified = True

        if verbose and modified:
            print('after essential boundary conditions:')
            print('A^(%d):\n' % e, A_e);  print('b^(%d):' % e, b_e)

        # Assemble
        for r in range(n):
            for s in range(n):
                A[dof_map[e][r], dof_map[e][s]] += A_e[r,s]
            b[dof_map[e][r]] += b_e[r]

    timing['assemble'] = time.clock() - t0
    t1 = time.clock()
    c = np.linalg.solve(A, b)
    timing['solve'] = time.clock() - t1
    if verbose:
        print('Global A:\n', A); print('Global b:\n', b)
        print('Solution c:\n', c)
    return c, A, b, timing


def finite_element1D(
    vertices, cells, dof_map,     # mesh
    essbc,                        # essbc[globdof]=value
    ilhs,
    irhs,
    blhs=lambda e, phi, r, s, X, x, h: 0,
    brhs=lambda e, phi, r, X, x, h: 0,
    intrule='GaussLegendre',
    verbose=False,
    ):
    N_e = len(cells)
    N_n = np.array(dof_map).max() + 1

    import scipy.sparse
    A = scipy.sparse.dok_matrix((N_n, N_n))
    b = np.zeros(N_n)

    timing = {}
    t0 = time.clock()

    for e in range(N_e):
        Omega_e = [vertices[cells[e][0]], vertices[cells[e][1]]]
        h = Omega_e[1] - Omega_e[0]

        d = len(dof_map[e]) - 1  # Polynomial degree
        # Compute all element basis functions and their derivatives
        phi = basis(d)

        if verbose:
            print('e=%2d: [%g,%g] h=%g d=%d' % \
                  (e, Omega_e[0], Omega_e[1], h, d))

        # Element matrix and vector
        n = d+1  # No of dofs per element
        A_e = np.zeros((n, n))
        b_e = np.zeros(n)

        # Integrate over the reference cell
        if intrule == 'GaussLegendre':
            points, weights = GaussLegendre(d+1)
        elif intrule == 'NewtonCotes':
            points, weights = NewtonCotes(d+1)

        for X, w in zip(points, weights):
            detJ = h/2
            x = affine_mapping(X, Omega_e)
            dX = detJ*w

            # Compute contribution to element matrix and vector
            for r in range(n):
                for s in range(n):
                    A_e[r,s] += ilhs(e, phi, r, s, X, x, h)*dX
                b_e[r] += irhs(e, phi, r, X, x, h)*dX

        # Add boundary terms
        for r in range(n):
            for s in range(n):
                A_e[r,s] += blhs(e, phi, r, s, X, x, h)
            b_e[r] += brhs(e, phi, r, X, x, h)

        if verbose:
            print('A^(%d):\n' % e, A_e);  print('b^(%d):' % e, b_e)

        # Incorporate essential boundary conditions
        modified = False
        for r in range(n):
            global_dof = dof_map[e][r]
            if global_dof in essbc:
                # local dof r is subject to an essential condition
                value = essbc[global_dof]
                # Symmetric modification
                b_e -= value*A_e[:,r]
                A_e[r,:] = 0
                A_e[:,r] = 0
                A_e[r,r] = 1
                b_e[r] = value
                modified = True

        if verbose and modified:
            print('after essential boundary conditions:')
            print('A^(%d):\n' % e, A_e);  print('b^(%d):' % e, b_e)

        # Assemble
        for r in range(n):
            for s in range(n):
                A[dof_map[e][r], dof_map[e][s]] += A_e[r,s]
            b[dof_map[e][r]] += b_e[r]

    import scipy.sparse.linalg
    t1 = time.clock()
    timing['assemble'] = t1 - t0
    c = scipy.sparse.linalg.spsolve(A.tocsr(), b, use_umfpack=True)
    timing['solve'] = time.clock() - t1
    if verbose:
        print('Global A:\n', A)
        print('Nonzero (i,j) in A:', list(A.keys()))
        print('Global b:\n', b);  print('Solution c:\n', c)
    return c, A, b, timing



#print basis(d=1, symbolic=True)

def mesh_uniform(N_e, d, Omega=[0,1], symbolic=False):
    """
    Return a 1D finite element mesh on Omega with N_e elements of
    the polynomial degree d. The elements have uniform length.
    Return vertices (vertices), local vertex to global
    vertex mapping (cells), and local to global degree of freedom
    mapping (dof_map).
    If symbolic is True, the vertices are expressed as rational
    sympy expressions with the symbol h as element length.
    """
    vertices = np.linspace(Omega[0], Omega[1], N_e + 1).tolist()
    if d == 0:
        dof_map = [[e] for e in range(N_e)]
    else:
        dof_map = [[e*d + i for i in range(d+1)] for e in range(N_e)]
    cells = [[e, e+1] for e in range(N_e)]
    return vertices, cells, dof_map

def define_cases(name=None):
    C = 0.5;  D = 2;  L = 4  # constants for case 'cubic'
    cases = {
        # u''=0 on (0,1), u(0)=0, u(1)=1 => u(x)=x
        'linear': {
            'Omega': [0,1],
            'ilhs': lambda e, phi, r, s, X, x, h:
            phi[1][r](X, h)*phi[1][s](X, h),
            'irhs': lambda e, phi, r, X, x, h: 0,
            'blhs': lambda e, phi, r, s, X, x, h: 0,
            'brhs': lambda e, phi, r, X, x, h: 0,
            'u_L': 0,
            'u_R': 1,
            'u_exact': lambda x: x,
            },
        # -u''=2 on (0,1), u(0)=0, u(1)=0 => u(x)=x(1-x)
        'quadratic': {
            'Omega': [0,1],
            'ilhs': lambda e, phi, r, s, X, x, h:
            phi[1][r](X, h)*phi[1][s](X, h),
            'irhs': lambda e, phi, r, X, x, h:
            2*phi[0][r](X),
            'blhs': lambda e, phi, r, s, X, x, h: 0,
            'brhs': lambda e, phi, r, X, x, h: 0,
            'u_L': 0,
            'u_R': 0,
            'u_exact': lambda x: x*(1-x),
            },
        # -u''=f(x) on (0,L), u'(0)=C, u(L)=D
        # f(x)=x: u = D + C*(x-L) + (1./6)*(L**3 - x**3)
        'cubic': {
            'Omega': [0,L],
            'ilhs': lambda e, phi, r, s, X, x, h:
            phi[1][r](X, h)*phi[1][s](X, h),
            'irhs': lambda e, phi, r, X, x, h:
            x*phi[0][r](X),
            'blhs': lambda e, phi, r, s, X, x, h: 0,
            'brhs': lambda e, phi, r, X, x, h:
            -C*phi[0][r](-1) if e == 0 else 0,
            'u_R': D,
            'u_exact': lambda x: D + C*(x-L) + (1./6)*(L**3 - x**3),
            'min_d': 1,  # min d for exact finite element solution
            },
        }
    if name is None:
        return cases
    else:
        return {name: cases[name]}

def test_finite_element1D():
    """Solve 1D test problems."""
    cases = define_cases()
    verbose = False

    for name in cases:
        case = cases[name]
        for N_e in [3]:
            for d in [1, 2, 3, 4]:
                # Do we need a minimum d to get exact
                # numerical solution?
                if d < case.get('min_d', 0):
                    continue
                vertices, cells, dof_map = \
                    mesh_uniform(N_e=N_e, d=d, Omega=case['Omega'],
                                 symbolic=False)
                N_n = np.array(dof_map).max() + 1
                # Assume uniform mesh
                x = np.linspace(
                    case['Omega'][0], case['Omega'][1], N_n)

                essbc = {}
                if 'u_L' in case:
                    essbc[0] = case['u_L']
                if 'u_R' in case:
                    essbc[dof_map[-1][-1]] = case['u_R']

                c, A, b, timing = finite_element1D_naive(
                    vertices, cells, dof_map, essbc,
                    case['ilhs'], case['irhs'],
                    case['blhs'], case['brhs'],
                intrule='GaussLegendre',
                verbose=verbose)
                # Compare with exact solution
                tol = 1E-12
                diff = (case['u_exact'](x) - c).max()
                msg = 'naive:  case "%s", N_e=%d, d=%d, diff=%g' % \
                      (name, N_e, d, diff)
                print(msg, 'assemble: %.2f' % timing['assemble'], \
                      'solve: %.2f' % timing['solve'])
                assert diff < tol, msg

                c, A, b, timing = finite_element1D(
                    vertices, cells, dof_map, essbc,
                    case['ilhs'], case['irhs'],
                    case['blhs'], case['brhs'],
                intrule='GaussLegendre',
                verbose=verbose)

                # Compare with exact solution
                diff = (case['u_exact'](x) - c).max()
                msg = 'sparse: case "%s", N_e=%d, d=%d, diff=%g' % \
                      (name, N_e, d, diff)
                print(msg, 'assemble: %.2f' % timing['assemble'], \
                      'solve: %.2f' % timing['solve'])
                assert diff < tol, msg

def investigate_efficiency():
    """Compare sparse and dense matrix versions of the FE algorithm."""
    case = define_cases('linear')

    for N_e in [300000, 1000000]:
        for d in [1, 2, 3]:
            vertices, cells, dof_map = \
                mesh_uniform(N_e=3, d=3, Omega=[0,1],
                             symbolic=False)
            N_n = np.array(dof_map).max() + 1
            x = np.linspace(0, 1, N_n)

            essbc = {}
            essbc[0] = case['u_L']
            essbc[dof_map[-1][-1]] = case['u_R']

            c, A, b, timing = finite_element1D_naive(
                vertices, cells, dof_map, essbc,
                case['ilhs'], case['irhs'],
                case['blhs'], case['brhs'],
            intrule='GaussLegendre',
            verbose=False)
            msg = 'naive:  N_e=%d, d=%d: assemble=%.2e solve=%.2e' % \
                  (N_e, d, timing['assemble'], timing['solve'])
            print(msg)

            c, A, b, timing = finite_element1D(
                vertices, cells, dof_map, essbc,
                case['ilhs'], case['irhs'],
                case['blhs'], case['brhs'],
            intrule='GaussLegendre',
            verbose=False)
            msg = 'sparse: N_e=%d, d=%d: assemble=%.2e solve=%.2e' % \
                  (N_e, d, timing['assemble'], timing['solve'])
            print(msg)

if __name__ == '__main__':
    test_finite_element1D()
    #investigate_efficiency()
