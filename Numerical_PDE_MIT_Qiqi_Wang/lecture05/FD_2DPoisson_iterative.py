"""
finite difference 2D poisson equation with iterative method.

u[i,j] = 0.25*( u[i+1,j] + u[i-1,j] + u[i,j+1] + u[i,j-1] ) + 0.25*f*dx*dx
from: Computational Physics, P468 Eq(19.31)

date : 2018-05-21
author: kouui
"""
################################################################################

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import numba as nb

################################################################################
@nb.jit(nb.void(nb.float64[:],nb.float64[:],nb.float64,nb.float64))
def solve2DPoisson_iter(u,f, dx2, dy2):
    """
    this function updates 2D poisson equation using Gauss-Seidel method.
    boudary condition: the 2 rows and 2 columns at boudary are all fixed.

    input:
        u   : 2darray, unknown in Poisson equation
        r   : 2darray, source term in Poisson equation
        dx2 : dx**2
        dy2 : dy**2

    output: void

    """
    for i in range(1,u.shape[0]-1):
        for j in range(1,u.shape[1]-1):
            u[i,j] = ( dy2*(u[i,j-1]+u[i,j+1]) + dx2*(u[i-1,j]+u[i+1,j]) + dx2*dy2*f[i,j] ) *0.5 / (dx2+dy2)

################################################################################

if __name__=="__main__":

    N = 101
    u = np.zeros((N,N), np.double)
    f = np.ones((N,N), np.double)
    x = np.linspace(0,1,N)
    y = np.linspace(0,1,N)
    dx2, dy2 = (x[1]-x[0])*(x[1]-x[0]), (y[1]-y[0])*(y[1]-y[0])
    errorMax = 1.

    #-- boundary
    #u[:,0] = 1.

    #-- solve
    iter = 1
    print(" iter \t errorMax")
    print("-"*20)
    while errorMax > 1.99E-6:
        if iter%100==0: print("{0:5d} \t {1:.2E} ".format(iter, errorMax))
        u_old = u.copy()
        solve2DPoisson_iter(u,f, dx2, dy2)
        errorMax = (abs(u[1:-1,1:-1] - u_old[1:-1,1:-1])).max()
        iter += 1

    #-- visualization
    X,Y = np.meshgrid(x,y)
    if True:
        fig = plt.figure(figsize=(4,4),dpi=150)
        ax = Axes3D(fig)
        #ax.plot_wireframe(X,Y,u, color="r")
        ax.plot_surface(X,Y,u, cmap="jet")
        plt.show()
