"""
2017NumericalMethodsOfPDE, lectureX2
solve 2D poisson equation with Gauss-Seidel iteration method

date : 2018-06-18
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt
import numba as nb

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

if __name__ == "__main__":

    #-- read image data

    filename = "./anime.png"
    img = plt.imread(filename)[:,:,0]
    # 1024 x 1024, img ranging from 0 to 1
    print("shape of array img is: {}".format(img.shape))
    nyImg, nxImg = img.shape
    assert nyImg==nxImg, "should be the same!"

    #-- construct source term b
    n = nyImg + 2
    x_exact = np.zeros((n,n),dtype=np.double)
    x_exact[1:-1,1:-1] = img[:,:]

    b = Laplacian(x_exact)
    print("source term b constructed.")

    #-- solve equation with the constructed source term b

    x = np.zeros((n,n),dtype=np.double)
    print("start calculating x:")
    fig, ax = plt.subplots(1,1, figsize=(6,6), dpi=100)
    im = ax.imshow(x, cmap="gray")
    plt.show(block=False)
    for k in range(10):

        GaussSeidel2D_oneStep(x,b)

        #-- method 1 to animate
        #ax.cla()
        #im = ax.imshow(x, cmap="gray")
        #plt.pause(0.1)

        #-- method 2 to animate
        im.set_data(x)
        im.norm.vmin, im.norm.vmax = x.min(), x.max()
        plt.pause(0.1)

    print("finish calculating x.")

    #plt.imshow(b, cmap="gray", vmin=-200,vmax=200)
    plt.show()
