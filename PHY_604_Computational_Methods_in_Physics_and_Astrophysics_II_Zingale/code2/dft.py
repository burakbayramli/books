from __future__ import print_function

import math
import numpy as np
import matplotlib.pyplot as plt


def dft(f_n):
    """ perform a discrete Fourier transform.  We use the same
        conventions as NumPy's FFT"""
    
    N = len(f_n)

    # allocate space for the frequency components -- they are, in general,
    # complex
    f_k = np.zeros( (N), dtype=np.complex128)

    for k in range(N):        
        # create f_k by looping over all the f_n real-space data points
        # this will create a complex number 

        for n in range(N):
            f_k[k] += f_n[n]*np.exp(-2.0*math.pi*1j*n*k/N)

    return f_k


npts = 50

f_0 = 0.2

xmax = 50.0
xx = np.linspace(0.0, xmax, npts, endpoint=False)

f_n = np.sin(2.0*math.pi*f_0*xx)

f_k = dft(f_n)

print(f_k[0])

# compute k
k = np.arange(npts)/xmax

plt.plot(k, f_k.real, label="Re(F(k))")
plt.plot(k, f_k.imag, ls=":", label="Im(F(k))")
plt.legend(frameon=False, fontsize="small")
plt.savefig("dft.png")

