'''
This module implements FEM in 2D with a Cartesian array of square elements.
It computes the linear stiffness matrix.
Every element is a square of size (2 scale)x(2 scale).
The elastic modulus of the elements is provided by an input array.

Example:
    from numpy import *
    from cartfem2d import FEM2D

    nx, ny = 10, 2
    fem = FEM2D(0.3, nx, ny)    # Poisson ratio and rectangular domain shape
    E = 1E6 * ones([ny, nx])    # elastic modulus array of shape [ny,nx]
    K = fem.K_matrix(E)         # linear stiffness matrix

Qiqi Wang, May 2017
'''

from __future__ import division, print_function

from scipy.sparse import *
import scipy.sparse.linalg as splinalg

from numpy import *
from numpy.polynomial import legendre

import matplotlib.pyplot as plt

def _shpfun(x, y):
    'value of the shape function'
    return array([(1-x)*(1-y), (1+x)*(1-y), (1+x)*(1+y), (1-x)*(1+y)]) / 4

def _ddx(x, y):
    'x derivative of shape function'
    return array([-(1-y), (1-y), (1+y), -(1+y)]) / 4

def _ddy(x, y):
    'y derivative of shape function'
    return array([-(1-x), -(1+x), (1+x), (1-x)]) / 4

def _Axx(x, y):
    '''linear component in xx Cauchy-Green finite strain tensor field.
    sigma_xx = dot(_Axx(x,y), U)
    where U=[u00 v00 u10 v10 u11 v11 u01 v01]
    '''
    return ravel(transpose([_ddx(x, y), zeros(4)]))

def _Ayy(x, y):
    'linear component in yy Cauchy-Green strain tensor. See doc for _Axx'
    return ravel(transpose([zeros(4), _ddy(x, y)]))

def _Axy(x, y):
    'linear component in xy Cauchy-Green strain tensor. See doc for _Axx'
    return ravel(transpose([_ddy(x, y), _ddx(x, y)]))

def stiffness_element_matrices(nu):
    '''
    Quadratic and cubic components of the elastic energy. Example:
        Q, C = element_matrix(0.3) # Poisson ratio
        UE_times_2 = dot(dot(Q, U), U) + dot(dot(dot(C, U), U), U)
    where U=[u00 v00 u10 v10 u11 v11 u01 v01]
    There is a quatic component but not computed here.
    '''
    n = 4
    xg, wg = legendre.leggauss(n)
    xg, yg = meshgrid(xg, xg)
    wg = outer(wg, wg)
    Q0 = zeros([2*n, 2*n])
    Q1 = zeros([2*n, 2*n])
    for i in range(n):
        for j in range(n):
            axx = _Axx(xg[i,j], yg[i,j])
            ayy = _Ayy(xg[i,j], yg[i,j])
            axy = _Axy(xg[i,j], yg[i,j])
            Q0 += wg[i,j] * (outer(axx, axx) + outer(ayy, ayy)
                             + 1/2 * outer(axy, axy))
            Q1 += wg[i,j] * (outer(axx, ayy) + outer(ayy, axx)
                             - 1/2 * outer(axy, axy))
    premul = 1 / (1 - nu**2)
    return premul * (Q0 + nu * Q1)

class FEM2D:
    '''
    The main interface of this module. See module doc for usage.
    '''
    def __init__(self, nu, nelx, nely, scale):
        '''
        nu: Poisson ratio
        shape: (nelx,nely) Note each direction has one less element than nodes
        '''
        self.Q = stiffness_element_matrices(nu)
        # the following is a Python adaptation of what's in top88.m
        nodenrs = reshape(arange((1+nelx)*(1+nely)), [1+nelx,1+nely])
        edofVec = ravel(2 * nodenrs[:-1,:-1])
        self.edofMat = edofVec[:,newaxis] + \
                ravel(array([1, nely+2, nely+1, 0])[:,newaxis] * 2 + arange(2))
        self.iK = ravel(kron(self.edofMat, ones([8,1], dtype=int)))
        self.jK = ravel(kron(self.edofMat, ones([1,8], dtype=int)))
        self.nelx, self.nely = nelx, nely
        self.scale = scale

    @property
    def xy(self):
        'return (nx+1, ny+1, 2) array. xy[:,:,0] is x; xy[:,:,1] is y.'
        x, y = meshgrid(arange(self.nelx+1),
                        self.nely - arange(self.nely+1), indexing='ij')
        return 2 * self.scale * array([x, y])

    def K_matrix(self, E):
        '''
        Linear stiffness matrix, square of size 2(nelx+1)(nely+1)
        Matrix is singular. Slice it before inverting (rhs can be force).
        '''
        assert E.shape == (self.nelx, self.nely)
        K = csr_matrix((kron(ravel(E), ravel(self.Q)), (self.iK, self.jK)))
        return (K + K.T) / 2

# ------------------------------ helper functions ---------------------------- #

def plot_deformation(fem, U):
    nelx, nely = fem.nelx, fem.nely
    U = U.reshape([nelx+1, nely+1, 2])
    x, y = fem.xy
    dx = U[:,:,0]
    dy = U[:,:,1]
    plt.plot(x, y, 'k', lw=.5)
    plt.plot(x.T, y.T, 'k', lw=.5)
    plt.plot(x+dx, y+dy, 'r', lw=.5)
    plt.plot((x+dx).T, (y+dy).T, 'r', lw=.5)
    plt.axis('scaled')
    margin = max(x.max() - x.min(), y.max() - y.min()) * 0.2
    plt.xlim([-x.min() - margin, x.max() + margin])
    plt.ylim([-x.min() - margin, y.max() + margin])
    plt.show()

def node_index_x(i, j, nelx, nely):
    if i < 0: i = nelx + 1 + i
    if j < 0: j = nely + 1 + j
    return (i * (nely + 1) + j) * 2

def node_index_y(i, j, nelx, nely):
    return node_index_x(i, j, nelx, nely) + 1

# --------------------------------- Solutions -------------------------------- #

E0 = 1.18E11
nu = 0.31

def compute_deformation(nelx, nely, scale=1):
    E = E0 * ones([nelx, nely])

    F = zeros(2*(nely+1)*(nelx+1))
    if nely % 2 == 0:
        F[node_index_y(-1, nely//2, nelx, nely)] = -8E4
    else:
        F[node_index_y(-1, nely//2, nelx, nely)] = -4E4
        F[node_index_y(-1, nely//2+1, nelx, nely)] = -4E4

    fixeddofs = set([node_index_x(0, j, nelx, nely) for j in range(nely+1)]
                  + [node_index_y(0, j, nelx, nely) for j in range(nely+1)])
    alldofs = set(arange(F.size))
    freedofs = array(sorted(set.difference(alldofs, fixeddofs)), int)

    fem = FEM2D(nu, nelx, nely, scale)
    K = fem.K_matrix(E)
    U = zeros_like(F)
    U[freedofs] = splinalg.spsolve(K[freedofs,:][:,freedofs], F[freedofs])
    return fem, U.reshape([nelx + 1, nely + 1, 2])

def question_2a():
    fem, U = compute_deformation(1, 1)
    plt.figure()
    plot_deformation(fem, U * 5E4)
    plt.title('Deformation exaggerated by a factor of 50,000')

plot_exaggeration = {2:20000, 4:5000, 10:1000}

def question_2b(N):
    fem, U = compute_deformation(N, 1)
    plt.figure()
    exagg = plot_exaggeration[N]
    plot_deformation(fem, U * exagg)
    plt.title('Deformation exaggerated by a factor of {}'.format(exagg))

def question_2c(N, M):
    fem, U = compute_deformation(N*M, M, 1./M)
    plt.figure()
    exagg = plot_exaggeration[N]
    plot_deformation(fem, U * exagg)
    plt.title('Deformation exaggerated by a factor of {}'.format(exagg))

if __name__ == '__main__':

    question_2a()
    for N in [2,4,10]:
        question_2b(N)
    for N in [2,4,10]:
        for M in [2,4,8]:
            question_2c(N,M)
