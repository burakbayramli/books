import numpy as np
import sympy as sym
import sys
import mpmath

def mesh_uniform(N_e, d, Omega=[0,1], symbolic=False):
    """
    Return a 1D finite element mesh on Omega with N_e elements of
    the polynomial degree d. The nodes are uniformly spaced.
    Return nodes (coordinates) and elements (connectivity) lists.
    If symbolic is True, the nodes are expressed as rational
    sympy expressions with the symbol h as element length.
    """
    if symbolic:
        h = sym.Symbol('h')  # element length
        dx = h*sym.Rational(1, d)  # node spacing
        nodes = [Omega[0] + i*dx for i in range(N_e*d + 1)]
    else:
        nodes = np.linspace(Omega[0], Omega[1], N_e*d + 1).tolist()
    elements = [[e*d + i for i in range(d+1)] \
                for e in range(N_e)]
    return nodes, elements


from Lagrange import Lagrange_polynomial, Chebyshev_nodes, Lagrange_polynomials


def basis(d, point_distribution='uniform', symbolic=True):
    """
    Return all local basis function phi as functions of the
    local point X in a 1D element with d+1 nodes.
    If symbolic=True, return symbolic expressions, else
    return Python functions of X.
    point_distribution can be 'uniform' or 'Chebyshev'.
    """
    X = sym.symbols('X')
    if d == 0:
        phi_sym = [1]
    else:
        if point_distribution == 'uniform':
            if symbolic:
                h = sym.Rational(1, d)  # node spacing
                nodes = [2*i*h - 1 for i in range(d+1)]
            else:
                nodes = np.linspace(-1, 1, d+1)
        elif point_distribution == 'Chebyshev':
            # Just numeric nodes
            nodes = Chebyshev_nodes(-1, 1, d)

        phi_sym = [Lagrange_polynomial(X, r, nodes)
                   for r in range(d+1)]
    # Transform to Python functions
    phi_num = [sym.lambdify([X], phi_sym[r], modules='numpy')
               for r in range(d+1)]
    return phi_sym if symbolic else phi_num


def affine_mapping(X, Omega_e):
    x_L, x_R = Omega_e
    return 0.5*(x_L + x_R) + 0.5*(x_R - x_L)*X

def locate_element_scalar(x, elements, nodes):
    """Return number of element containing point x. Scalar version."""
    for e, local_nodes in enumerate(elements):
        if nodes[local_nodes[0]] <= x <= nodes[local_nodes[-1]]:
            return e

def locate_element_vectorized(x, elements, nodes):
    """Return number of element containing point x. Vectorized version."""
    elements = np.asarray(elements)
    print(elements[:,-1])
    element_right_boundaries = nodes[elements[:,-1]]
    return searchsorted(element_right_boundaries, x)

# vectorized version for locating elements: numpy.searchsorted
#http://www.astropython.org/snippet/2010/11/Interpolation-without-SciPy


def u_glob(U, elements, nodes, resolution_per_element=51):
    """
    Compute (x, y) coordinates of a curve y = u(x), where u is a
    finite element function: u(x) = sum_i of U_i*phi_i(x).
    (The solution of the linear system is in U.)
    Method: Run through each element and compute curve coordinates
    over the element.
    """
    x_patches = []
    u_patches = []
    for e in range(len(elements)):
        Omega_e = (nodes[elements[e][0]], nodes[elements[e][-1]])
        local_nodes = elements[e]
        d = len(local_nodes) - 1
        phi = basis(d)
        X = np.linspace(-1, 1, resolution_per_element)
        x = affine_mapping(X, Omega_e)
        x_patches.append(x)
        u_element = 0
        for r in range(len(local_nodes)):
            i = local_nodes[r]  # global node number
            u_element += U[i]*phi[r].evalf(X)
        u_patches.append(u_element)
    x = np.concatenate(x_patches)
    u = np.concatenate(u_patches)
    return x, u


def element_matrix(phi, Omega_e, symbolic=True):
    n = len(phi)
    A_e = sym.zeros(n, n)
    X = sym.Symbol('X')
    if symbolic:
        h = sym.Symbol('h')
    else:
        h = Omega_e[1] - Omega_e[0]
    detJ = h/2  # dx/dX
    for r in range(n):
        for s in range(r, n):
            A_e[r,s] = sym.integrate(phi[r]*phi[s]*detJ, (X, -1, 1))
            A_e[s,r] = A_e[r,s]
    return A_e

def element_vector(f, phi, Omega_e, symbolic=True):
    n = len(phi)
    b_e = sym.zeros(n, 1)
    # Make f a function of X (via f.subs to avoid floats from lambdify)
    X = sym.Symbol('X')
    if symbolic:
        h = sym.Symbol('h')
    else:
        h = Omega_e[1] - Omega_e[0]
    x = (Omega_e[0] + Omega_e[1])/2 + h/2*X  # mapping
    f = f.subs('x', x)  # or subs(sym.Symbol('x'), x)?
    detJ = h/2  # dx/dX
    for r in range(n):
        if symbolic:
            I = sym.integrate(f*phi[r]*detJ, (X, -1, 1))
        if not symbolic or isinstance(I, sym.Integral):
            print('numerical integration of', f*phi[r]*detJ)
            # Ensure h is numerical
            h = Omega_e[1] - Omega_e[0]
            detJ = h/2
            integrand = sym.lambdify([X], f*phi[r]*detJ, 'mpmath')
            I = mpmath.quad(integrand, [-1, 1])
        b_e[r] = I
    return b_e

def exemplify_element_matrix_vector(f, d, symbolic=True):
    phi = basis(d)
    print('phi basis (reference element):\n', phi)
    Omega_e = [0.1, 0.2]
    A_e = element_matrix(phi, Omega_e=Omega_e, symbolic=symbolic)
    if symbolic:
        h = sym.Symbol('h')
        Omega_e=[1*h, 2*h]
    b_e = element_vector(f, phi, Omega_e=Omega_e,
                         symbolic=symbolic)
    print('Element matrix:\n', A_e)
    print('Element vector:\n', b_e)

def assemble(nodes, elements, phi, f, symbolic=True):
    N_n, N_e = len(nodes), len(elements)
    if symbolic:
        A = sym.zeros(N_n, N_n)
        b = sym.zeros(N_n, 1)    # note: (N_n, 1) matrix
    else:
        A = np.zeros((N_n, N_n))
        b = np.zeros(N_n)
    for e in range(N_e):
        Omega_e = [nodes[elements[e][0]], nodes[elements[e][-1]]]

        A_e = element_matrix(phi, Omega_e, symbolic)
        b_e = element_vector(f, phi, Omega_e, symbolic)

        for r in range(len(elements[e])):
            for s in range(len(elements[e])):
                A[elements[e][r],elements[e][s]] += A_e[r,s]
            b[elements[e][r]] += b_e[r]
    return A, b

def approximate(f, symbolic=False, d=1, N_e=4,
                Omega=[0, 1], filename='tmp'):
    phi = basis(d)
    print('phi basis (reference element):\n', phi)

    nodes, elements = mesh_uniform(N_e, d, Omega, symbolic)
    A, b = assemble(nodes, elements, phi, f, symbolic=symbolic)

    print('nodes:', nodes)
    print('elements:', elements)
    print('A:\n', A)
    print('b:\n', b)
    print(sym.latex(A, mode='plain'))
    #print sym.latex(b, mode='plain')

    if symbolic:
        c = A.LUsolve(b)
        c = [c[i,0] for i in range(c.shape[0])]
    else:
        c = np.linalg.solve(A, b)

    print('c:\n', c)

    print('Plain interpolation/collocation:')
    x = sym.Symbol('x')
    f = sym.lambdify([x], f, modules='numpy')
    try:
        f_at_nodes = [f(xc) for xc in nodes]
    except NameError as e:
        raise NameError('numpy does not support special function:\n%s' % e)
    print(f_at_nodes)

    if not symbolic and filename is not None:
        xf = np.linspace(Omega[0], Omega[1], 10001)
        U = np.asarray(c)  # c is a plain array
        xu, u = u_glob(U, elements, nodes)
        #import scitools.std as plt
        import matplotlib.pyplot as plt
        plt.plot(xu, u, '-',
                 xf, f(xf), '--')
        plt.legend(['u', 'f'])
        plt.savefig(filename + '.pdf')
        plt.savefig(filename + '.png')

    return c

def phi_glob(i, elements, nodes, resolution_per_element=41,
             derivative=0):
    """
    Compute (x, y) coordinates of the curve y = phi_i(x),
    where i is a global node number (used for plotting, e.g.).
    Method: Run through each element and compute the pieces
    of phi_i(x) on this element in the reference coordinate
    system. Adding up the patches yields the complete phi_i(x).
    """
    x_patches = []
    phi_patches = []
    for e in range(len(elements)):
        Omega_e = [nodes[elements[e][0]], nodes[elements[e][-1]]]
        local_nodes = elements[e]
        d = len(local_nodes) - 1
        X = np.linspace(-1, 1, resolution_per_element)
        if i in local_nodes:
            r = local_nodes.index(i)
            if derivative == 0:
                phi = phi_r(r, X, d)
            elif derivative == 1:
                phi = dphi_r(r, X, d)
                if phi is None:
                    return None, None
            phi_patches.append(phi)
            x = affine_mapping(X, Omega_e)
            x_patches.append(x)
        else:
            # i is not a node in the element, phi_i(x)=0
            x_patches.append(Omega_e)
            phi_patches.append([0, 0])
    x = np.concatenate(x_patches)
    phi = np.concatenate(phi_patches)
    return x, phi

def phi_r(r, X, d):
    """
    Return local basis function phi_r at local point X in
    a 1D element with d+1 nodes.
    """
    if d == 0:
        return np.ones_like(X)
    nodes = np.linspace(-1, 1, d+1)
    return Lagrange_polynomial(X, r, nodes)

def dphi_r(r, X, d, point_distribution='uniform'):
    """
    Return the derivative of local basis function phi_r at
    local point X in a 1D element with d+1 nodes.
    point_distribution can be 'uniform' or 'Chebyshev'.
    """
    # Strange function....
    if isinstance(X, np.ndarray):
        z = np.zeros(len(X))
    if d == 0:
        return 0 + z
    elif d == 1:
        if r == 0:
            return -0.5 + z
        elif r == 1:
            return 0.5 + z
    elif d == 2:
        if r == 0:
            return X - 0.5
        elif r == 1:
            return -2*X
        elif r == 2:
            return X + 0.5
    else:
        print('dphi_r only supports d=0,1,2, not %d' % d)
        return None


if __name__ == '__main__':
    pass
