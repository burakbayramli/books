# solve a 2-d Poisson equation by differentiating the discretized Poisson
# equation and then substituting in the inverse Fourier transform and solving
# for the amplitudes in Fourier space.
#
# This is the way that Garcia and NR do it.
#
# Note: we need a periodic problem for an FFT

from __future__ import print_function

import matplotlib.pyplot as plt
import numpy as np

# the analytic solution
def true(x,y):
    pi = np.pi
    return np.sin(2.0*pi*x)**2*np.cos(4.0*pi*y) + \
        np.sin(4.0*pi*x)*np.cos(2.0*pi*y)**2


# the righthand side
def frhs(x,y):
    pi = np.pi
    return 8.0*pi**2*np.cos(4.0*pi*y)*(np.cos(4.0*pi*x) - 
                                          np.sin(4.0*pi*x)) - \
           16.0*pi**2*(np.sin(4.0*pi*x)*np.cos(2.0*pi*y)**2 + 
                       np.sin(2.0*pi*x)**2 * np.cos(4.0*pi*y))



def fft_solve(N):

    Nx = Ny = N

    # create the domain
    xmin = 0.0
    xmax = 1.0

    ymin = 0.0
    ymax = 1.0

    dx = (xmax - xmin)/Nx
    dy = (ymax - ymin)/Ny

    x = (np.arange(Nx) + 0.5)*dx
    y = (np.arange(Ny) + 0.5)*dy

    x2d = np.repeat(x, Ny)
    x2d.shape = (Nx, Ny)

    y2d = np.repeat(y, Nx)
    y2d.shape = (Ny, Nx)
    y2d = np.transpose(y2d)

    # create the RHS
    f = frhs(x2d, y2d)

    # compatibility conditions require that the RHS sum to zero
    print("sum of RHS: ", np.sum(f))

    # FFT of RHS
    F = np.fft.fft2(f)

    # get the wavenumbers -- we need these to be physical, so multiply by Nx
    kx = Nx*np.fft.fftfreq(Nx)
    ky = Ny*np.fft.fftfreq(Ny)

    # make 2-d arrays for the wavenumbers
    kx2d = np.repeat(kx, Ny)
    kx2d.shape = (Nx, Ny)

    ky2d = np.repeat(ky, Nx)
    ky2d.shape = (Ny, Nx)
    ky2d = np.transpose(ky2d)

    # here the FFT frequencies are in the order 0 ... N/2-1, -N/2, ...
    # the 0 component is not a physical frequency, but rather it is the DC
    # signal.  Don't mess with it, since we'll divide by zero
    oldDC = F[0,0]
    F = 0.5*F*dx*dx/(np.cos(2.0*np.pi*kx2d/Nx) + 
                     np.cos(2.0*np.pi*ky2d/Ny) - 2.0)# + 1.e-20)

    F[0,0] = oldDC

    # transform back to real space
    fsolution = np.real(np.fft.ifft2(F))

    return fsolution, np.sqrt(dx*dx*np.sum( ( (fsolution - true(x2d,y2d))**2).flat))


if __name__ == "__main__":

    N = 64

    fsolution, err = fft_solve(N)

    plt.imshow(fsolution, origin="lower", interpolation="nearest")
    plt.colorbar()

    plt.tight_layout()

    plt.savefig("poissonFFT.png")

    print(N, err)



