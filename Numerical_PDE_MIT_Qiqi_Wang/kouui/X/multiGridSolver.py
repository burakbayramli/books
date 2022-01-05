"""
2017NumericalMethodsOfPDE, lectureX3
solve poisson equation with multi-grid method (recursion)

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

def Prolongate(Dx_coarse):
    nc = Dx_coarse.shape[0] - 1
    n = nc*2
    Dx_fine = np.zeros((n+1,n+1),dtype=np.double)
    Dx_fine[::2,::2] = Dx_coarse[:,:]
    Dx_fine[1:-1:2,::2] = 0.5*(Dx_coarse[:-1,:]+Dx_coarse[1:,:])
    Dx_fine[:,1:-1:2] = 0.5*(Dx_fine[:,:-2:2]+Dx_fine[:,2::2])
    return Dx_fine

def is_odd(num):
    return num & 0x1

def multiGrid(x, b, nIter):

    assert x.ndim==2, "should be 2 dimensional"
    assert x.shape[0]==x.shape[1], "should be equal aspect"
    assert is_odd(x.shape[0]), "x dimension should has odd pixels"

    n = x.shape[0] - 1
    dx = 1/n
    for k in range(nIter):
        #-- step 1: pre-smoothing
        for _ in range(4):
            GaussSeidel2D_oneStep(x,b)

        if n > 5:
            #-- step 2: residual, and go coarse
            r = b - Laplacian(x)
            restrictR = r[::2,::2].copy()

            #-- step 3: solve error equation
            deltaX_coarse = np.zeros(restrictR.shape, dtype=np.double)
            print("shape of deltaX_coarse: {}".format(deltaX_coarse.shape))
            multiGrid(deltaX_coarse, restrictR, 1)

            #-- step 4: go fine, and add it back to x
            deltaX_fine = Prolongate(deltaX_coarse)
            print("shape of deltaX_fine: {}".format(deltaX_fine.shape))
            x[:,:] += deltaX_fine[:,:]
        else:
            pass

        #-- step 5: post-smoothing
        for _ in range(4):
            GaussSeidel2D_oneStep(x,b)



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
    multiGrid(x, b, 1)


    #-- result visualization
    fig, axs = plt.subplots(1,3, figsize=(10,3), dpi=100)

    im0 = axs[0].imshow(x_exact, cmap="gray")
    axs[0].set_title("original image")

    im1 = axs[1].imshow(x, cmap="gray")
    im1.set_norm(im0.norm)
    axs[1].set_title("computed image")

    im2 = axs[2].imshow(x-x_exact, cmap="gray")
    im2.set_norm(im0.norm)
    axs[2].set_title("error")

    for ax in axs:
        ax.set_axis_off()
    plt.show()
