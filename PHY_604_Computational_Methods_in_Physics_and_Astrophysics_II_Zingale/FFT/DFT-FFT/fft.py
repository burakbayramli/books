from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt


def fft(f_n):
    """ perform a discrete Fourier transform.  We use the same
        conventions as NumPy's FFT

        here,

              N-1         -2 pi i n k /N
        F_k = sum  f   exp
              i=0   n

    """

    N = len(f_n)

    if N == 1:
        return f_n
    else:

        # split into even and odd and find the FFTs of each half
        f_even = f_n[0:N:2]
        f_odd = f_n[1:N:2]

        F_even = fft(f_even)
        F_odd = fft(f_odd)

        # combine them.  Each half has N/2 wavenumbers, but due to
        # periodicity, we can compute N wavenumbers
        omega = np.exp(-2*np.pi*1j/N)

        # allocate space for the frequency components -- they are, in general,
        # complex
        F_k = np.zeros((N), dtype=np.complex128)

        for k in range(N//2):
            F_k[k] = F_even[k] + omega**k * F_odd[k]
            F_k[N//2 + k] = F_even[k] - omega**k * F_odd[k]

    return F_k


if __name__ == "__main__":
    npts = 64

    f_0 = 0.2

    xmax = 50.0
    xx = np.linspace(0.0, xmax, npts, endpoint=False)

    f_n = np.sin(2.0*np.pi*f_0*xx)

    f_k = fft(f_n)

    print(f_k[0])

    # compute k
    k = np.arange(npts)/xmax

    plt.plot(k, f_k.real, label="Re(F(k))")
    plt.plot(k, f_k.imag, ls=":", label="Im(F(k))")
    plt.legend(frameon=False, fontsize="medium")

    plt.savefig("fft.png", dpi=150)
