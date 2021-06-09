import numpy as np
import matplotlib.pyplot as plt

def mesh(N_e, d, Omega=[0,1]):
    """
    Return a 1D finite element mesh on Omega with N_e elements of
    the polynomial degree d. The nodes are uniformly spaced.
    Return nodes (coordinates) and elements (connectivity) arrays.
    """
    # or should we just have end-nodes and leave it up to info
    # elsewhere to have internal nodes and other dofs?
    nodes = np.linspace(Omega[0], Omega[1], N_e*d + 1)
    elements = np.asarray([[e*d + i for i in range(d+1)] \
                           for e in range(N_e)], int)
    return nodes, elements


from Lagrange import Lagrange_polynomial, Lagrange_polynomials

def phi_r(r, X, d):
    """
    Return local basis function phi_r at local point X in
    a 1D element with d+1 nodes.
    """
    nodes = np.linspace(-1, 1, d+1)
    return Lagrange_polynomial(X, r, nodes)

def phi_r(r, X, d, point_distribution='uniform'):
    """
    Return local basis function phi_r at local point X in
    a 1D element with d+1 nodes.
    point_distribution can be 'uniform' or 'Chebyshev'.
    """
    if point_distribution == 'uniform':
        nodes = np.linspace(-1, 1, d+1)
    elif point_distribution == 'Chebyshev':
        nodes = Chebyshev_nodes(-1, 1, d)
    return Lagrange_polynomial(X, r, nodes)

def basis(X, d=1):
    """Return the finite element basis in 1D of degree d."""
    phi = [phi_r(r, X, d) for r in range(d+1)]
    return phi

def affine_mapping(X, Omega_e):
    x_L, x_R = Omega_e
    return 0.5*(x_L + x_R) + 0.5*(x_R - x_L)*X

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
        Omega_e = (nodes[elements[e,0]], nodes[elements[e,-1]])
        local_nodes = elements[e,:]
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

def element_matrix(d, Omega_e, numint):
    n = d+1
    A_e = np.zeros((n, n))
    h = Omega_e[1] - Omega_e[0]
    detJ = h/2  # dx/dX
    for j in range(len(numint[0])):
        Xj, wj = numint[0][j], numint[1][j]
        phi = basis(Xj, d)
        for r in range(n):
            for s in range(r, n):
                A_e[r,s] += phi[r](Xj)*phi[s](Xj)*detJ*wj
                A_e[s,r] += A_e[r,s]
    return A_e

def element_vector(f, d, Omega_e, intrule):
    n = d+1
    b_e = np.zeros(n)
    h = Omega_e[1] - Omega_e[0]
    detJ = h/2  # dx/dX
    for j in range(len(numint[0])):
        Xj, wj = numint[0][j], numint[1][j]
        phi = basis(Xj, d)
    for r in range(n):
        b_e[r,s] += f(Xj)*phi[r](Xj)*detJ*wj
    return b_e

def assemble(nodes, elements, d, f, numint):
    N_n, N_e = len(nodes), len(elements)
    A = np.zeros((N_n, N_n))
    b = np.zeros((N_n, 1))
    for e in range(N_e):
        Omega_e = [nodes[elements[e,0]], nodes[elements[e,-1]]]
        A_e = element_matrix(d, Omega_e, numint)
        b_e = element_vector(f, d, Omega_e, numint)
        #print 'element', e
        #print b_e
        for r in range(len(elements[e,:])):
            for s in range(len(elements[e])):
                A[elements[e,r],elements[e,s]] += A_e[r,s]
            b[elements[e,r]] += b_e[r]
    return A, b


def approximate(f, d=1, N_e=4, numint='Gauss-Legendre2',
                filename='tmp.eps'):
    nodes, elements = mesh(N_e, d, [0, 1])
    A, b = assemble(nodes, elements, d, f, numint)
        
    c = A.LUsolve(b)

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
    f_at_nodes = [f(xc) for xc in nodes]
    print(f_at_nodes)
    if not symbolic:
        xf = np.linspace(0, 1, 10001)
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
    eval(cmd)

