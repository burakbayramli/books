import math, cmath


def radix2fft(x, sign=-1.0):
    """ Computes the FFT of a list of values.
    Assumes the number of values is a power of 2. """

    N = len(x)
    if N == 1:  # Fourier transform of one point is that point
        return [x[0]]
    else:  # compute half-size ffts of even and odd data
        X = radix2fft(x[0::2], sign)
        X += radix2fft(x[1::2], sign)
    omega = cmath.exp(math.copysign(2.0, sign)*cmath.pi*1j/N)
    omegak = 1.0
    for k in range(N//2):  # butterfly
        tmp = omegak*X[k+N//2]
        (X[k], X[k+N//2]) = (X[k]+tmp, X[k]-tmp)
        omegak *= omega
    return X


def fft(x, sign=-1.0):
    """ Computes the FFT of a list of values.
    Requires the number of values to be a power of 2. """

    # make sure length of x is a power of 2
    N = len(x)
    if N & N-1 != 0:
        raise ValueError('number of points must be a power of 2')
    return radix2fft(x, sign)


def ifft(X, sign=1.0):
    """ Computes the inverse FFT of a list of values.
    Requires the number of values to be a power of 2. """

    N = len(X)
    x = fft(X, sign)
    for j in range(N):
        x[j] /= N
    return x


def dft(x, sign=-1.0):
    """ Computes the DFT of a list of values using the slow O(N^2) method. """

    N = len(x)
    y = [0.0]*N
    for k in range(N):
        for j in range(N):
            y[k] += x[j]*cmath.exp(math.copysign(2.0, sign)*cmath.pi*1j*j*k
                                   /float(N))
    return y


def hann(N):
    """ Create a Hann window. """

    fac = 2.0*math.pi/(N-1)
    wss = 0.0
    w = [0.0]*N
    for j in range(N):
        w[j] = 0.5*(1.0-math.cos(fac*j))
        wss += w[j]**2
    return (w, wss)


def hamming(N):
    """ Create a Hamming window. """

    fac = 2.0*math.pi/(N-1)
    wss = 0.0
    w = [0.0]*N
    for i in range(N):
        w[i] = 0.54-0.46*math.cos(fac*j)
        wss += w[j]**2
    return (w, wss)


def bartlett(N):
    """ Create a Bartlett window. """

    mid = (N-1.0)/2.0
    wss = 0.0
    w = [0.0]*N
    for j in range(N):
        w[j] = 1.0-abs(j-mid)/mid
        wss += w[j]**2
    return (w, wss)


def periodogram(x, window='Rectangular'):
    """ Computes the periodogram of a list of values.
    Requires the number of values to be a power of 2.
    Returns only the positive frequencies. """

    N = len(x)
    # construct window
    if window == 'Rectangular':
        (w, wss) = ([1.0]*N, N)
    elif window == 'Hann':
        (w, wss) = hann(N)
    elif window == 'Hamming':
        (w, wss) = hamming(N)
    elif window == 'Bartlett':
        (w, wss) = bartlett(N)
    else:
        raise ValueError('unrecognized window type')
    # apply window to a copy of the data
    y = [0.0]*N
    for j in range(N):
        y[j] = w[j]*x[j]
    # fft windowed data and compute power
    Y = fft(y)
    Y[0] = Y[0].real**2/wss
    for k in range(1, N//2):
        Y[k] = (Y[k].real**2+Y[k].imag**2)/wss
        Y[k] += (Y[N-k].real**2+Y[N-k].imag**2)/wss
    Y[N//2] = Y[N//2].real**2/wss
    return Y[:N//2+1]


def psdwelch(x, N, window='Rectangular'):
    """ Computes the power spectral density of a list of values using
    Welch's method with overlapping segments of length N.
    Requires N to be a power of 2.
    Returns only the positive frequencies. """

    M = len(x)
    K = 2*M//N-1  # number of segments
    S = [0.0]*(N//2+1)  # the power spectral density
    # compute the running mean of the power spectrum
    for l in range(K):
        P = periodogram(x[l*N//2:N+l*N//2], window)
        for k in range(N/2+1):
            S[k] = (P[k]+l*S[k])/(l+1)
    return S
