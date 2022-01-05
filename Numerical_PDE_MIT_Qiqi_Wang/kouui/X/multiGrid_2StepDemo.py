"""
2017NumericalMethodsOfPDE, lectureX3
solve poisson equation with multi-grid method (only 2 steps)

date : 2018-06-18
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt
import numba as nb

@nb.jit(nb.float64[:](nb.float64[:]))
def Laplacian(x):
    n = x.shape[0]-1
    dx = 1/n
    LapX = np.zeros(x.shape, dtype=np.double)
    for i in range(1,n):
        for j in range(1,n):
            LapX[i,j] = (x[i-1,j]+x[i+1,j]+x[i,j-1]+x[i,j+1] - 4*x[i,j])/ dx/dx

    return LapX

@nb.jit(nb.void(nb.float64[:],nb.float64[:]))
def GaussSeidel2D_oneStep(x,b):
    dx = 1/(x.shape[1]-1)
    for i in range(1,x.shape[0]-1):
        for j in range(1,x.shape[1]-1):
            x[i,j] = -0.25 * ( dx*dx*b[i,j] - (x[i-1,j]+x[i+1,j]+x[i,j-1]+x[i,j+1]) )

def Restrict(r):
    n = r.shape[0] - 1
    nc = int(n/2)
    rc = np.zeros((nc+1,nc+1),dtype=np.double)
    for i in range(1,nc):
        for j in range(1,nc):
            rc[i,j] = r[(i-1)*2+1, (j-1)*2+1]
    return rc

def Prolongate(Dx_coarse):
    nc = Dx_coarse.shape[0] - 1
    n = nc*2
    Dx_fine = np.zeros((n+1,n+1),dtype=np.double)
    Dx_fine[::2,::2] = Dx_coarse[:,:]
    Dx_fine[1:-1:2,::2] = 0.5*(Dx_coarse[:-1,:]+Dx_coarse[1:,:])
    Dx_fine[:,1:-1:2] = 0.5*(Dx_fine[:,:-2:2]+Dx_fine[:,2::2])
    return Dx_fine


if __name__ == "__main__":

    #-- read image data

    filename = "./anime.png"
    img = plt.imread(filename)[:,:,0]
    # 1024 x 1024, img ranging from 0 to 1
    print("shape of array img is: {}".format(img.shape))
    nyImg, nxImg = img.shape
    assert nyImg==nxImg, "should be the same!"

    #-- construct source term b
    n = nyImg + 1
    x_exact = np.zeros((n,n),dtype=np.double)
    x_exact[1:-1,1:-1] = img[1:,1:]

    b = Laplacian(x_exact)
    print("source term b constructed.")

    #-- step 1

    x = np.zeros((n,n),dtype=np.double)
    print("shape of x: {}".format(x.shape))
    for k in range(10):
        GaussSeidel2D_oneStep(x,b)

    #-- step 2
    r = b - Laplacian(x)

    #restrictR = Restrict(r)
    restrictR = r[::2,::2].copy()

    #-- step 3
    deltaX_coarse = np.zeros(restrictR.shape, dtype=np.double)
    print("shape of deltaX_coarse: {}".format(deltaX_coarse.shape))
    for k in range(50):
        GaussSeidel2D_oneStep(deltaX_coarse,restrictR)

    #-- step 4
    deltaX_fine = Prolongate(deltaX_coarse)
    print("shape of deltaX_fine: {}".format(deltaX_fine.shape))
    x[:,:] += deltaX_fine[:,:]
    for k in range(10):
        GaussSeidel2D_oneStep(x,b)


    plt.imshow(x, cmap="gray")
    plt.show()
