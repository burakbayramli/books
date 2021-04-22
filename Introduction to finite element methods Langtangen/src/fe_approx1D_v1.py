import numpy as np
import sympy as sym
import matplotlib.pyplot as plt

def mesh(N_e, d, Omega=[0,1]):
    """
    Return a 1D finite element mesh on Omega with N_e elements of
    the polynomial degree d. The nodes are uniformly spaced.
    Return nodes (coordinates) and elements (connectivity) lists.
    """
    nodes = np.linspace(Omega[0], Omega[1], N_e*d + 1).tolist()
    elements = [[e*d + i for i in range(d+1)] \
                for e in range(N_e)]
    return nodes, elements

def mesh_symbolic(N_e, d, Omega=[0,1]):
    """
    Return a 1D finite element mesh on Omega with N_e elements of
    the polynomial degree d. The nodes are uniformly spaced.
    Return nodes (coordinates) and elements (connectivity)
    lists, using symbols for the coordinates (rational expressions
    with h as the uniform element length).
    """
    h = sym.Symbol('h')  # element length
    dx = h*sym.Rational(1, d)  # node spacing
    nodes = [Omega[0] + i*dx for i in range(N_e*d + 1)]
    elements = [[e*d + i for i in range(d+1)] \
                for e in range(N_e)]
    return nodes, elements

def mesh2(N_e, d, Omega=[0,1]):
    """
    Return a 1D finite element mesh on Omega with N_e elements of
    the polynomial degree d. The nodes are uniformly spaced.
    Return vertices (vertices), local vertex to global
    vertex mapping (cells), and local to global degree of freedom
    mapping (dof_map).
    """
    vertices = np.linspace(Omega[0], Omega[1], N_e + 1).tolist()
    doc_map = [[e*d + i for i in range(d+1)] for e in range(N_e)]
    cells = [[e, e+1] for e in range(N_e)]
    return vertices, cells, dof_map
    # Not yet used

from Lagrange import Lagrange_polynomial, Lagrange_polynomials

def phi_r(r, X, d):
    """
    Return local basis function phi_r at local point X in
    a 1D element with d+1 nodes.
    """
    if isinstance(X, sym.Symbol):
        # Use sym.Rational and integers for nodes
        # (to maximize nice-looking output)
        h = sym.Rational(1, d)
        nodes = [2*i*h - 1 for i in range(d+1)]
    else:
        # X is numeric: use floats for nodes
        nodes = np.linspace(-1, 1, d+1)
    return Lagrange_polynomial(X, r, nodes)

def phi_r(r, X, d, point_distribution='uniform'):
    """
    Return local basis function phi_r at local point X in
    a 1D element with d+1 nodes.
    point_distribution can be 'uniform' or 'Chebyshev'.
    """
    if point_distribution == 'uniform':
        if isinstance(X, sym.Symbol):
            # Use sym.Rational and integers for nodes
            # (to maximize nice-looking output)
            h = sym.Rational(1, d)
            nodes = [2*i*h - 1 for i in range(d+1)]
        else:
            # X is numeric: use floats for nodes
            nodes = np.linspace(-1, 1, d+1)
    elif point_distribution == 'Chebyshev':
        nodes = Chebyshev_nodes(-1, 1, d)
    return Lagrange_polynomial(X, r, nodes)

def basis(d=1):
    """Return the finite element basis in 1D of degree d."""
    X = sym.Symbol('X')
    phi = [phi_r(r, X, d) for r in range(d+1)]
    return phi

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

def phi_glob(i, elements, nodes, resolution_per_element=41):
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
        Omega_e = (nodes[elements[e][0]], nodes[elements[e][-1]])
        local_nodes = elements[e]
        d = len(local_nodes) - 1
        X = np.linspace(-1, 1, resolution_per_element)
        if i in local_nodes:
            r = local_nodes.index(i)
            phi = phi_r(r, X, d)
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

def u_glob(U, elements, nodes, resolution_per_element=51):
    """
    Compute (x, y) coordinates of a curve y = u(x), where u is a
    finite element function: u(x) = sum_i of U_i*phi_i(x).
    Method: Run through each element and compute cordinates
    over the element.
    """
    x_patches = []
    u_patches = []
    for e in range(len(elements)):
        Omega_e = (nodes[elements[e][0]], nodes[elements[e][-1]])
        local_nodes = elements[e]
        d = len(local_nodes) - 1
        X = np.linspace(-1, 1, resolution_per_element)
        x = affine_mapping(X, Omega_e)
        x_patches.append(x)
        u_element = 0
        for r in range(len(local_nodes)):
            i = local_nodes[r]  # global node number
            u_element += U[i]*phi_r(r, X, d)
        u_patches.append(u_element)
    x = np.concatenate(x_patches)
    u = np.concatenate(u_patches)
    return x, u

def element_matrix(phi, Omega_e, symbolic=True):
    n = len(phi)
    A_e = sym.zeros((n, n))
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
    b_e = sym.zeros((n, 1))
    # Make f a function of X (via f.subs to avoid real numbers from lambdify)
    X = sym.Symbol('X')
    if symbolic:
        h = sym.Symbol('h')
    else:
        h = Omega_e[1] - Omega_e[0]
    x = (Omega_e[0] + Omega_e[1])/2 + h/2*X  # mapping
    f = f.subs('x', x)  # or subs(sym.Symbol('x'), x)?
    detJ = h/2  # dx/dX
    for r in range(n):
        I = sym.integrate(f*phi[r]*detJ, (X, -1, 1))
        if isinstance(I, sym.Integral):
            print('numerical integration of', f*phi[r]*detJ)
            # Ensure h is numerical
            h = Omega_e[1] - Omega_e[0]
            detJ = h/2
            integrand = sym.lambdify([X], f*phi[r]*detJ)
            I = sym.mpmath.quad(integrand, [-1, 1])
        b_e[r] = I
    return b_e


def assemble(nodes, elements, phi, f, symbolic=True):
    N_n, N_e = len(nodes), len(elements)
    A = sym.zeros((N_n, N_n))
    b = sym.zeros((N_n, 1))
    for e in range(N_e):
        Omega_e = [nodes[elements[e][0]], nodes[elements[e][-1]]]
        A_e = element_matrix(phi, Omega_e, symbolic)
        b_e = element_vector(f, phi, Omega_e, symbolic)
        #print 'element', e
        #print b_e
        for r in range(len(elements[e])):
            for s in range(len(elements[e])):
                A[elements[e][r],elements[e][s]] += A_e[r,s]
            b[elements[e][r]] += b_e[r]
    return A, b


def approximate(f, symbolic=False, d=1, N_e=4,
                Omega=[0, 1], filename='tmp.eps'):
    phi = basis(d)
    print('phi basis (reference element):\n', phi)
    # Exemplify element matrix and vector
    Omega_e = [0.1, 0.2]
    A_e = element_matrix(phi, Omega_e=Omega_e,
                         symbolic=symbolic, numint=numint)
    if symbolic:
        h = sym.Symbol('h')
        Omega_e=[1*h, 2*h]
    b_e = element_vector(f, phi, Omega_e=Omega_e,
                         symbolic=symbolic, numint=numint)
    print('Element matrix:\n', A_e)
    print('Element vector:\n', b_e)

    if symbolic:
        nodes, elements = mesh_symbolic(N_e, d, Omega)
    else:
        nodes, elements = mesh(N_e, d, Omega)
    A, b = assemble(nodes, elements, phi, f, symbolic=symbolic)

    print('nodes:', nodes)
    print('elements:', elements)
    print('A:\n', A)
    print('b:\n', b)
    print(sym.latex(A, mode='plain'))
    #print sym.latex(b, mode='plain')

    c = A.LUsolve(b)
    print('c:\n', c)
    print('Plain interpolation:')
    x = sym.Symbol('x')
    f = sym.lambdify([x], f, modules='numpy')
    try:
        f_at_nodes = [f(xc) for xc in nodes]
    except NameError as e:
        raise NameError('numpy does not support special function:\n%s' % e)
    print(f_at_nodes)
    if not symbolic:
        xf = np.linspace(Omega[0], Omega[1], 10001)
        U = np.asarray(c)
        xu, u = u_glob(U, elements, nodes)
        plt.plot(xu, u, 'r-',
                 xf, f(xf), 'b-')
        plt.legend('u', 'f')
        plt.savefig(filename)


# Extended versions with numerical integration (Midpoint, Trap., Simpson)
# (note that these functions overwrite those above!)

def element_matrix(phi, Omega_e, symbolic=True, numint=None):
    n = len(phi)
    A_e = sym.zeros((n, n))
    X = sym.Symbol('X')
    if symbolic:
        h = sym.Symbol('h')
    else:
        h = Omega_e[1] - Omega_e[0]
    detJ = h/2  # dx/dX
    if numint is None:
        for r in range(n):
            for s in range(r, n):
                A_e[r,s] = sym.integrate(phi[r]*phi[s]*detJ, (X, -1, 1))
                A_e[s,r] = A_e[r,s]
    else:
        #phi = [sym.lambdify([X], phi[r]) for r in range(n)]
        # Do instead a phi_rj = phi[r].subs(X, Xj) to avoid real numbers
        for r in range(n):
            for s in range(r, n):
                for j in range(len(numint[0])):
                    Xj, wj = numint[0][j], numint[1][j]
                    phi_rj = phi[r].subs(X, Xj)
                    phi_sj = phi[s].subs(X, Xj)
                    A_e[r,s] += phi_rj*phi_sj*detJ*wj
                A_e[s,r] = A_e[r,s]
    return A_e

def element_vector(f, phi, Omega_e, symbolic=True, numint=None):
    n = len(phi)
    b_e = sym.zeros((n, 1))
    # Make f a function of X (via f.subs to avoid real numbers from lambdify)
    X = sym.Symbol('X')
    if symbolic:
        h = sym.Symbol('h')
    else:
        h = Omega_e[1] - Omega_e[0]
    x = (Omega_e[0] + Omega_e[1])/2 + h/2*X  # mapping
    f = f.subs('x', x)
    detJ = h/2
    if numint is None:
        for r in range(n):
            I = sym.integrate(f*phi[r]*detJ, (X, -1, 1))
            if isinstance(I, sym.Integral):
                print('numerical integration of', f*phi[r]*detJ)
                # Ensure h is numerical
                h = Omega_e[1] - Omega_e[0]
                detJ = h/2
                integrand = sym.lambdify([X], f*phi[r]*detJ)
                I = sym.mpmath.quad(integrand, [-1, 1])
            b_e[r] = I
    else:
        #phi = [sym.lambdify([X], phi[r]) for r in range(n)]
        # f contains h from the mapping, substitute X with Xj
        # instead of f = sym.lambdify([X], f)
        for r in range(n):
            for j in range(len(numint[0])):
                Xj, wj = numint[0][j], numint[1][j]
                fj = f.subs(X, Xj)
                phi_rj = phi[r].subs(X, Xj)
                b_e[r] += fj*phi_rj*detJ*wj
    return b_e

def assemble(nodes, elements, phi, f, symbolic=True, numint=None):
    N_n, N_e = len(nodes), len(elements)
    A = sym.zeros((N_n, N_n))
    b = sym.zeros((N_n, 1))
    for e in range(N_e):
        Omega_e = [nodes[elements[e][0]], nodes[elements[e][-1]]]
        A_e = element_matrix(phi, Omega_e, symbolic, numint)
        b_e = element_vector(f, phi, Omega_e, symbolic, numint)
        #print 'element', e
        #print b_e
        for r in range(len(elements[e])):
            for s in range(len(elements[e])):
                A[elements[e][r],elements[e][s]] += A_e[r,s]
            b[elements[e][r]] += b_e[r]
    return A, b

def approximate(f, symbolic=False, d=1, N_e=4, numint=None,
                Omega=[0, 1], filename='tmp.eps'):
    if symbolic:
        if numint == 'Trapezoidal':
            numint = [[sym.S(-1), sym.S(1)], [sym.S(1), sym.S(1)]]  # sumpy integers
        elif numint == 'Simpson':
            numint = [[sym.S(-1), sym.S(0), sym.S(1)],
                      [sym.Rational(1,3), sym.Rational(4,3), sym.Rational(1,3)]]
        elif numint == 'Midpoint':
            numint = [[sym.S(0)],  [sym.S(2)]]
        else:
            numint = None
    else:
        if numint == 'Trapezoidal':
            numint = [[-1, 1], [1, 1]]
        elif numint == 'Simpson':
            numint = [[-1, 0, 1], [1./3, 4./3, 1./3]]
        elif numint == 'Midpoint':
            numint = [[0], [2]]
        else:
            numint = None

    phi = basis(d)
    print('phi basis (reference element):\n', phi)
    integration_msg = """
    Symbolic integration failed, and then numerical integration
    encountered an undefined symbol (because of the symbolic expressions):
    %s"""
    # Exemplify element matrix and vector
    Omega_e = [0.1, 0.2]
    A_e = element_matrix(phi, Omega_e=Omega_e,
                         symbolic=symbolic, numint=numint)
    if symbolic:
        h = sym.Symbol('h')
        Omega_e=[1*h, 2*h]
    try:
        b_e = element_vector(f, phi, Omega_e=Omega_e,
                             symbolic=symbolic, numint=numint)
    except NameError as e:
        raise NameError(integration_msg % e)
    print('Element matrix:\n', A_e)
    print('Element vector:\n', b_e)

    if symbolic:
        try:
            nodes, elements = mesh_symbolic(N_e, d, Omega)
        except NameError as e:
            raise NameError(integration_msg % e)
    else:
        nodes, elements = mesh(N_e, d, Omega)

    A, b = assemble(nodes, elements, phi, f,
                    symbolic=symbolic, numint=numint)

    print('nodes:', nodes)
    print('elements:', elements)
    print('A:\n', A)
    print('b:\n', b)
    #print sym.latex(A, mode='plain')
    #print sym.latex(b, mode='plain')

    c = A.LUsolve(b)
    print('c:\n', c)
    print('Plain interpolation:')
    x = sym.Symbol('x')
    f = sym.lambdify([x], f, modules='numpy')
    try:
        f_at_nodes = [f(xc) for xc in nodes]
    except NameError as e:
        raise NameError('numpy does not support special function:\n%s' % e)
    print(f_at_nodes)
    if not symbolic and filename is not None:
        xf = np.linspace(Omega[0], Omega[1], 10001)
        U = np.asarray(c)
        xu, u = u_glob(U, elements, nodes)
        plt.plot(xu, u, 'r-',
                 xf, f(xf), 'b-')
        plt.legend('u', 'f')
        plt.savefig(filename)


if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print("""Usage %s function arg1 arg2 arg3 ...""" % sys.argv[0])
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print(cmd)
    x = sym.Symbol('x')  # needed in eval when expression f contains x
    eval(cmd)

