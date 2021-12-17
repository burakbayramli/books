from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt


def dft(f_n):
    """ perform a discrete Fourier transform.  We use the same
        conventions as NumPy's FFT"""

    N = len(f_n)

    # allocate space for the frequency components -- they are, in general,
    # complex
    f_k = np.zeros( (N), dtype=np.complex128)

    n = np.arange(N)

    for k in range(N):
        # create f_k by summing over all the f_n real-space data points
        # this will create a complex number

        f_k[k] = np.sum(f_n*np.exp(-2.0*np.pi*1j*n*k/N))

    return f_k


if __name__ == "__main__":
    npts = 64

    nu_0 = 0.2
    xmax = 50.0
    xx = np.linspace(0.0, xmax, npts, endpoint=False)

    f_n = np.sin(2.0*np.pi*nu_0*xx)

    f_k = dft(f_n)

    print(f_k[0])

    # compute nu_k
    k = np.arange(npts)/xmax

    plt.plot(k, f_k.real, label=r"Re($\mathcal{F}_k$)")
    plt.plot(k, f_k.imag, ls=":", label=r"Im($\mathcal{F}_k$)")

    plt.xlabel(r"$\nu_k$")
    plt.ylabel(r"$\mathcal{F}_k$")
    plt.legend(frameon=False, fontsize="medium")

    plt.savefig("dft.png", dpi=150)
