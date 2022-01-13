import pylab
import numpy
import math

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


def singleFreqSine(npts):

    # a pure sine with no phase shift will result in pure imaginary
    # signal
    f_0 = 0.2

    xmax = 10.0/f_0

    xx = numpy.linspace(0.0, xmax, npts, endpoint=False)

    # input frequency
    f_0 = 0.2

    f = numpy.sin(2.0*math.pi*f_0*xx)
    
    return xx, f

def singleFreqSinePlusShift(npts):

    # a pure sine with no phase shift will result in pure imaginary
    # signal
    f_0 = 0.2

    xmax = 10.0/f_0

    xx = numpy.linspace(0.0, xmax, npts, endpoint=False)

    # input frequency
    f_0 = 0.2

    f = numpy.sin(2.0*math.pi*f_0*xx + math.pi/4)
    
    return xx, f

def twoFreqSine(npts):

    # a pure sine with no phase shift will result in pure imaginary
    # signal
    f_0 = 0.2
    f_1 = 0.5

    xmax = 10.0/f_0

    xx = numpy.linspace(0.0, xmax, npts, endpoint=False)

    # input frequency
    f_0 = 0.2

    f = 0.5*(numpy.sin(2.0*math.pi*f_0*xx) + numpy.sin(2.0*math.pi*f_1*xx))
    
    return xx, f

def singleFreqCosine(npts):

    # a pure cosine with no phase shift will result in pure real
    # signal
    f_0 = 0.2

    xmax = 10.0/f_0

    xx = numpy.linspace(0.0, xmax, npts, endpoint=False)

    # input frequency
    f_0 = 0.2

    f = numpy.cos(2.0*math.pi*f_0*xx)
    
    return xx, f

def plotFFT(xx, f, outfile):

    pylab.clf()

    pylab.rc("font", size=9)

    npts = len(xx)

    # Forward transform: f(x) -> F(k)
    fk = numpy.fft.rfft(f)

    # Normalization -- the '2' here comes from the fact that we are
    # neglecting the negative portion of the frequency space, since
    # the FFT of a real function contains redundant information, so 
    # we are only dealing with 1/2 of the frequency space.
    norm = 2.0/npts

    fk = fk*norm

    # element 0 of fk is the DC component -- we don't want to plot that
    fk_r = fk.real
    fk_i = fk.imag

    # the fftfreq returns the postive and negative (and 0) frequencies
    # the newer versions of numpy (>=1.8) have an rfftfreq() function
    # that really does what we want.
    k = numpy.fft.fftfreq(len(xx))[range(0,npts/2+1)]

    # the last element is negative, because of the symmetry, but should
    # be positive (see http://docs.scipy.org/doc/numpy-dev/reference/generated/numpy.fft.rfftfreq.html)
    k[-1] *= -1

    kfreq = k*npts/max(xx)


    # Inverse transform: F(k) -> f(x) -- without the normalization
    fkinv = numpy.fft.irfft(fk/norm)

    pylab.subplot(411)

    pylab.plot(xx, f)
    pylab.xlabel("x")
    pylab.ylabel("f(x)")


    pylab.subplot(412)

    pylab.plot(kfreq, fk_r, label=r"Re($\mathcal{F}$)")
    pylab.plot(kfreq, fk_i, ls=":", label=r"Im($\mathcal{F}$)")
    pylab.xlabel(r"$\nu_k$")
    pylab.ylabel("F(k)")

    pylab.legend(fontsize="small", frameon=False)

    pylab.subplot(413)

    pylab.plot(kfreq, numpy.abs(fk))
    pylab.xlabel(r"$\nu_k$")
    pylab.ylabel(r"|F(k)|")


    pylab.subplot(414)

    pylab.plot(xx, fkinv.real)
    pylab.xlabel(r"$\nu_k$")
    pylab.ylabel(r"inverse F(k)")

    pylab.tight_layout()

    pylab.savefig(outfile)



#-----------------------------------------------------------------------------

npts = 256


# FFT of sine
xx, f = singleFreqSine(npts)
plotFFT(xx, f, "fft-sine.png")


# FFT of cosine
xx, f = singleFreqCosine(npts)
plotFFT(xx, f, "fft-cosine.png")

# FFT of sine with pi/4 phase
xx, f = singleFreqSinePlusShift(npts)
plotFFT(xx, f, "fft-sine-phase.png")

# FFT of two sines
xx, f = twoFreqSine(npts)
plotFFT(xx, f, "fft-two-sines.png")

