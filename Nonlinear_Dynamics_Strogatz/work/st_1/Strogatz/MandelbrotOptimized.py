from numba import jit, vectorize, guvectorize, float64, complex64, int32, float32
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import colors

norm =colors.PowerNorm(0.9)
#From https://www.ibm.com/developerworks/community/blogs/jfp/entry/How_To_Compute_Mandelbrodt_Set_Quickly?lang=en
@jit(int32(complex64, int32))
def mandelbrot(c,maxiter):
    nreal = 0
    real = 0
    imag = 0
    for n in range(maxiter):
        nreal = real*real - imag*imag + c.real
        imag = 2* real*imag + c.imag
        real = nreal;
        if real * real + imag * imag > 4.0:
            return n
    return 0

@guvectorize([(complex64[:], int32[:], int32[:])], '(n),()->(n)',target='parallel')
def mandelbrot_numpy(c, maxit, output):
    maxiter = maxit[0]
    for i in range(c.shape[0]):
        output[i] = mandelbrot(c[i],maxiter)
        
def mandelbrot_set2(xmin,xmax,ymin,ymax,width,height,maxiter):
    r1 = np.linspace(xmin, xmax, width, dtype=np.float32)
    r2 = np.linspace(ymin, ymax, height, dtype=np.float32)
    c = r1 + r2[:,None]*1j
    n3 = mandelbrot_numpy(c,maxiter)
    return (r1,r2,n3.T) 

xmin, xmax, xn = -2.25, +0.75, 3000/2
ymin, ymax, yn = -1.25, +1.25, 3000/2
maxiter = 1000

r1,r2,M = mandelbrot_set2(xmin,xmax,ymin,ymax,int(xn),int(yn),maxiter)
fig,ax = plt.subplots(figsize=(12,8))
ax.set_axis_off()    
ax.set_title(r"The Mandelbrot Set for $Z_{n + 1} = Z_n^2 + c$ where $c \in \mathbb{C}$")
ax.imshow(M,
                    aspect='equal',
                    cmap='nipy_spectral',
                    norm=norm,
                    interpolation='spline36')                
plt.show()
#plt.savefig('/Users/Llewelyn_home/Dropbox/Computation in Python/Chaos and nonlinear/Mandelbrot2.png')