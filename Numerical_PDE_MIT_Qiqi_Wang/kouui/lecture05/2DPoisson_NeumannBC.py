"""
2017NumericalMethodsOfPDE, lecture5-6
finite difference 2D poisson equation with Neumann boundary consition

du/dy = 0 at y=0, i.e., at i=0

now we need to include row 0

date : 2018-05-22
author: kouui
"""

import matplotlib.pyplot as plt
import numpy as np
import scipy.sparse as sps
from scipy.sparse.linalg import dsolve
from mpl_toolkits.mplot3d import Axes3D


if __name__ == '__main__':
    N = 100
    x = np.linspace(0, 1, N+1)
    y = np.linspace(0, 1, N+1)
    dx = x[1] - x[0]
    dy = y[1] - y[0]

    #-- block matrice
    temp = np.ones(N-2)
    A_diag = np.eye(N-1) * (-2*(1/dx**2+1/dy**2)) + np.diag(temp,+1) * 1/dx**2 + np.diag(temp,-1) * 1/dx**2
    A_offd = np.eye(N-1) * 1/dy**2

    #-- sparse matrix
    NN = (N-1)*(N-1)
    A_lil = sps.lil_matrix((NN+(N-1),NN+(N-1)),dtype=np.double)
    for i in range(N-1):
        st = i*(N-1)
        ed1 = (i+1)*(N-1)
        ed2 = (i+2)*(N-1)
        A_lil[st:ed1, st:ed1] = A_diag
        A_lil[st:ed1, ed1:ed2] = A_offd
        A_lil[ed1:ed2, st:ed1] = A_offd
    A_lil[ed1:ed2, ed1:ed2] = A_diag
    A_lil[0:(N-1),(N-1):2*(N-1)] *= 2

    #-- solve equation
    A_csr = A_lil.tocsr()
    f = np.ones(NN+(N-1))
    u = dsolve.spsolve(A_csr, -f, use_umfpack=True).reshape(N,N-1)

    #-- visualization
    if False:
        X, Y = np.meshgrid(x[1:-1],y[:-1])
        print(X.shape)
        fig = plt.figure(figsize=(4,4),dpi=150)
        ax = Axes3D(fig)
        #ax.plot_wireframe(X,Y,u, color="r")
        ax.plot_surface(X,Y,u, cmap="jet")
        plt.show()
