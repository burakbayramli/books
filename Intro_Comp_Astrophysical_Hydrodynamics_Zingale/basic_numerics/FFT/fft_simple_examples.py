from __future__ import print_function

import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
import math

mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'


# see: http://glowingpython.blogspot.com/2011/08/how-to-plot-frequency-spectrum-with.html
# and
# http://docs.scipy.org/doc/numpy/reference/routines.fft.html

# Since our input data is real, the negative frequency components
# don't include any new information, and are not interesting to us.
# The rfft routines understand this, and rfft takes n real points and
# returns n/2+1 complex output points.  The corresponding inverse
# knows this, and acts accordingly.
#
# these are the routines we want for real valued data
#
# note that the scipy version of rfft returns that data differently
#
# M. Zingale (2013-03-03)


def single_freq_sine(npts, xmax, f_0):

    # a pure sine with no phase shift will result in pure imaginary
    # signal

    xx = np.linspace(0.0, xmax, npts, endpoint=False)
    return xx, np.sin(2.0*math.pi*f_0*xx)

def single_freq_sine_plus_shift(npts, xmax, f_0):

    # a pure sine with no phase shift will result in pure imaginary
    # signal
    xx = np.linspace(0.0, xmax, npts, endpoint=False)
    return xx, np.sin(2.0*math.pi*f_0*xx + math.pi/4)

def two_freq_sine(npts, xmax, f_0, f_1):

    # a pure sine with no phase shift will result in pure imaginary
    # signal
    xx = np.linspace(0.0, xmax, npts, endpoint=False)
    f = 0.5*(np.sin(2.0*math.pi*f_0*xx) + np.sin(2.0*math.pi*f_1*xx))
    return xx, f

def single_freq_cosine(npts, xmax, f_0):

    # a pure cosine with no phase shift will result in pure real
    # signal
    xx = np.linspace(0.0, xmax, npts, endpoint=False)
    f = np.cos(2.0*math.pi*f_0*xx)
    return xx, f

def plot_FFT(xx, xmax, f, outfile):

    plt.clf()

    plt.rc("font", size=10)

    npts = len(xx)

    # Forward transform: f(x) -> F(k)
    fk = np.fft.rfft(f)

    # Normalization -- the '2' here comes from the fact that we are
    # neglecting the negative portion of the frequency space, since
    # the FFT of a real function contains redundant information, so 
    # we are only dealing with 1/2 of the frequency space.
    #
    # technically, we should only scale the 0 bin by N, since k=0 is
    # not duplicated -- we won't worry about that for these plots
    norm = 2.0/npts

    fk = fk*norm

    fk_r = fk.real
    fk_i = fk.imag

    # the fftfreq returns the postive and negative (and 0) frequencies
    # the newer versions of numpy (>=1.8) have an rfftfreq() function
    # that really does what we want -- we'll use that here.
    k = np.fft.rfftfreq(npts)

    # to make these dimensional, we need to divide by dx.  Note that
    # max(xx) is not the true length, since we didn't have a point
    # at the endpoint of the domain.
    kfreq = k*npts/(max(xx) + xx[1])

    # Inverse transform: F(k) -> f(x) -- without the normalization
    fkinv = np.fft.irfft(fk/norm)

    plt.subplot(411)

    plt.plot(xx, f)
    plt.xlabel("x")
    plt.ylabel("$f(x)$")

    plt.xlim(0, xmax)

    plt.subplot(412)

    plt.plot(kfreq, fk_r, label=r"Re($\mathcal{F}$)")
    plt.plot(kfreq, fk_i, ls=":", label=r"Im($\mathcal{F}$)")
    plt.xlabel(r"$k$")
    plt.ylabel("$\mathcal{F}_k$")

    plt.legend(fontsize="small", frameon=False, ncol=2, loc="upper right")

    plt.subplot(413)

    plt.plot(kfreq, np.abs(fk))
    plt.xlabel(r"$k$")
    plt.ylabel(r"$|\mathcal{F}_k|$")


    plt.subplot(414)

    plt.plot(xx, fkinv.real)
    plt.xlabel(r"$x$")
    plt.ylabel(r"$\mathcal{F}^{-1}(\mathcal{F}_k)$")

    plt.xlim(0, xmax)

    plt.tight_layout()

    plt.savefig(outfile)



#-----------------------------------------------------------------------------

npts = 256  #64  #256

f_0 = 0.2

xmax = 10.0/f_0
    
# FFT of sine
xx, f = single_freq_sine(npts, xmax, f_0)
plot_FFT(xx, xmax, f, "fft-sine.pdf")


# FFT of cosine
xx, f = single_freq_cosine(npts, xmax, f_0)
plot_FFT(xx, xmax, f, "fft-cosine.pdf")

# FFT of sine with pi/4 phase
xx, f = single_freq_sine_plus_shift(npts, xmax, f_0)
plot_FFT(xx, xmax, f, "fft-sine-phase.pdf")

# FFT of two sines
f_1 = 0.5
xx, f = two_freq_sine(npts, xmax, f_0, f_1)
plot_FFT(xx, xmax, f, "fft-two-sines.pdf")

