from __future__ import print_function

import matplotlib.pyplot as plt
import numpy as np
import math

# simple example of removing a frequency component from a
# two-frequency sine wave signal

# Since our input data is real, the negative frequency components
# don't include any new information, and are not interesting to us.
# The rfft routines understand this, and rfft takes n real points and
# returns n/2+1 complex output points.  The corresponding inverse
# knows this, and acts accordingly.
#
# these are the routines we want for real valued data

# note that the scipy version of rfft returns that data differently

# M. Zingale (2013-02-28)


def two_freq_sine(npts):

    # a pure sine with no phase shift will result in pure imaginary
    # signal
    f_0 = 0.2
    f_1 = 0.5

    xmax = 10.0/f_0

    # we call with endpoint=False -- if we include the endpoint, then for
    # a periodic function, the first and last point are identical -- this
    # shows up as a signal in the FFT.
    xx = np.linspace(0.0, xmax, npts, endpoint=False)
    f = 0.5*(np.sin(2.0*math.pi*f_0*xx) + np.sin(2.0*math.pi*f_1*xx))
    
    return xx, f




npts = 256

xx, f = two_freq_sine(npts)

# Forward transform: f(x) -> F(k)

# normalization factor: the 2 here comes from the fact that we neglect
# the negative portion of frequency space because our input function
# is real
norm = 2.0/npts   
fk = norm*np.fft.rfft(f)

ofk_r = fk.real.copy()
ofk_i = fk.imag.copy()


# just return the postive frequencies
k = np.fft.rfftfreq(len(xx))

# since we don't include the endpoint in xx, to normalize things, we need
# max(xx) + dx to get the true range
kfreq = k*npts/(max(xx) + xx[1])


# filter all frequencies > 0.4
fk[kfreq > 0.4] = 0.0


# element 0 of fk is the DC component
fk_r = fk.real
fk_i = fk.imag


# Inverse transform: F(k) -> f(x)    
fkinv = np.fft.irfft(fk/norm)

# PLOT
plt.rc("font", size=10)


plt.subplot(411)

plt.plot(xx, f)
plt.xlabel("x")
plt.ylabel("f(x)")


plt.subplot(412)

plt.plot(kfreq, ofk_r)
plt.plot(kfreq, ofk_i, ls=":")
plt.xlabel(r"$\nu_k$")
plt.ylabel("original F(k)")

#plt.xlim(0,1)

plt.subplot(413)

plt.plot(kfreq, fk_r)
plt.plot(kfreq, fk_i, ls=":")
plt.xlabel(r"$\nu_k$")
plt.ylabel("filtered F(k)")


plt.subplot(414)

plt.plot(xx, fkinv.real)
plt.xlabel("x")
plt.ylabel(r"inverse filtered F(k)")

plt.tight_layout()

f = plt.gcf()
f.set_size_inches(9.0,8.0)

plt.savefig("simple-filter.png", bbox_inches='tight', pad_inches=0.33)


