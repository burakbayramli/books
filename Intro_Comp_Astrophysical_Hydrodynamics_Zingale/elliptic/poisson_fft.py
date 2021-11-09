# solve a 2-d Poisson equation by differentiating the discretized
# Poisson equation and then substituting in the inverse Fourier
# transform and solving for the amplitudes in Fourier space.
#
# This is the standard way that texts deal with the Poisson equation
# (see, e.g., Garcia and NR)
#
# Note: we need a periodic problem for an FFT

from __future__ import print_function

import matplotlib.pyplot as plt
import numpy as np
import math
import matplotlib as mpl

# Use LaTeX for rendering
mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'
mpl.rcParams['font.size'] = 12
mpl.rcParams['legend.fontsize'] = 'large'
mpl.rcParams['figure.titlesize'] = 'medium'

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


def doit(Nx, Ny, do_plot=False):

    # create the domain -- cell-centered finite-difference / finite-volume
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

    # FFT of RHS
    F = np.fft.fft2(f)

    # get the wavenumbers -- we need these to be physical, so divide by dx
    kx = np.fft.fftfreq(Nx)/dx
    ky = np.fft.fftfreq(Ny)/dy

    # make 2-d arrays for the wavenumbers
    kx2d = np.repeat(kx, Ny)
    kx2d.shape = (Nx, Ny)

    ky2d = np.repeat(ky, Nx)
    ky2d.shape = (Ny, Nx)
    ky2d = np.transpose(ky2d)

    # here the FFT frequencies are in the order 0 ... N/2-1, -N/2, ...
    # the 0 component is not a physical frequency, but rather it is
    # the DC signal.  Don't mess with it, since we'll divide by zero
    oldDC = F[0,0]
    F = 0.5*F/( (np.cos(2.0*np.pi*kx2d/Nx) - 1.0)/dx**2 +
                (np.cos(2.0*np.pi*ky2d/Ny) - 1.0)/dy**2)

    F[0,0] = oldDC

    # transform back to real space
    fsolution = np.real(np.fft.ifft2(F))


    # since x is our row in the array, we transpose for the
    # plot
    if do_plot:
        plt.imshow(np.transpose(fsolution),
                   origin="lower", interpolation="nearest",
                   extent=[xmin, xmax, ymin, ymax])

        plt.xlabel("x")
        plt.ylabel("y")

        plt.colorbar()

        plt.tight_layout()

        plt.savefig("poisson_fft.pdf")
        

    # return the error, compared to the true solution
    return np.sqrt(dx*dx*np.sum( ( (fsolution - true(x2d,y2d))**2).flat))


N = [16, 32, 64, 128, 256]
plot = [False]*len(N)
plot[N.index(64)] = True

err = []
for n, p in zip(N, plot):
    err.append(doit(n, n, do_plot=p))


# plot the convergence
plt.clf()

N = np.array(N, dtype=np.float64)
err = np.array(err, dtype=np.float64)

print(N)
print(type(N))
print(err)

plt.scatter(N, err, marker="x", color="r")
plt.plot(N, err[0]*(N[0]/N)**2, "--", color="k", label="$\mathcal{O}(\Delta x^2)$")

ax = plt.gca()

ax.set_xscale('log')
ax.set_yscale('log')

plt.legend(frameon=False)

plt.xlabel("number of zones")
plt.ylabel("L2 norm of abs error")
                        
f = plt.gcf()
f.set_size_inches(5.0,5.0)

plt.savefig("fft-poisson-converge.pdf", bbox_inches="tight")

                                    


