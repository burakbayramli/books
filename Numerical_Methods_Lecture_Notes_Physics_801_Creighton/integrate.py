import interpolate, pylab, itertools


def trapstep(f, a, b, n, s):
    """ Perform the nth refinement of a trapezoid method and update sum.
    Here, f is the integrand, a and b are the limits of the integral,
    n is the step (0 on first call), and s is the sum so far. """

    if n == 0:
        return (b-a)*(f(b)+f(a))/2.0
    else:
        steps = 2**(n-1)
        dx = (b-a)/steps
        x0 = a+dx/2.0
        s = (s+dx*sum(f(x0+i*dx) for i in range(steps)))/2.0
        return s


def trapezoid(f, a, b, eps=1e-6):
    """ Trapezoidal method of quadrature to a specified accuracy.
    Here, f is the integrand, a and b are the limits of the integral,
    and eps is the desired fractional accuracy. """

    n = 0
    s = trapstep(f, a, b, n, 0.0)
    while True:
        n += 1
        s0 = s  # s0 is the previous value of the trapezoid sum
        s = trapstep(f, a, b, n, s)
        if abs(s-s0) < eps*abs(s0):
            return s


def simpson(f, a, b, eps=1e-6):
    """ Simpson's method of quadrature to a specified accuracy.
    Here, f is the integrand, a and b are the limits of the integral,
    and eps is the desired fractional accuracy. """

    n = 0
    q = s = trapstep(f, a, b, n, 0.0)
    while True:
        n += 1
        (q0, s0) = (q, s)  # save previous values
        s = trapstep(f, a, b, n, s)
        q = (4.0*s-s0)/3.0
        if abs(q-q0) < 0.25*eps*abs(q0):
            return q


def romberg(f, a, b, eps=1e-6):
    """ Romberg's method of quadrature to a specified accuracy.
    Here, f is the integrand, a and b are the limits of the integral,
    and eps is the desired fractional accuracy. """

    degree = 5  # degree of polynomial extrapolation
    n = 0
    s = [trapstep(f, a, b, n, 0.0)]
    xi = [1.0]
    while True:
        n += 1
        s += [trapstep(f, a, b, n, s[-1])]  # append new element
        xi += [xi[-1]/4.0]  # append new element
        if len(s) >= degree:
            ss = s[-degree:]  # last degree elements of s
            # extrapolate to dx = 0
            q = interpolate.polynomial(0.0, xi[-degree:], ss)
            dq = ss[1]-ss[0]  # error in extrapolation
            if abs(dq) < eps*abs(q):
                return q


def abscissas(N):
    """ Returns a list of (abscissa, weight) pairs suitable for
    Gaussian quadrature. """

    if N == 0:
        return []
    if N == 1:
        return [(0.0, 2.0)]
    # initial guess of root locations
    x = pylab.cos(pylab.pi*(4.0*pylab.arange(N)+3.0)/(4.0*N+2.0))
    # Newton's method
    eps = 1e-15
    while True:
        # generate Legendre polynomials using recurrence relation
        (p0, p1) = (1.0, x)
        for n in range(1,N):
            (p0, p1) = (p1, ((2.0*n+1.0)*x*p1-n*p0)/(n+1.0))
        dx = -(1.0-x**2)*p1/(N*p0)
        x += dx
        if max(abs(dx)) < eps:
            break
    w = 2.0*(1.0-x**2)/(N*p0)**2
    return [(x[n], w[n]) for n in range(N)]


def gaussian(f, a, b, N=5):
    """ Gaussian quadrature with a specified number of abscissas.
    Here, f is the integrand, a and b are the limits of the integral,
    and N is the number of abscissas. """

    fac = 0.5*(b-a)
    mid = 0.5*(b+a)
    return fac*sum(w*f(fac*x+mid) for (x,w) in abscissas(N))


def gaussiannd(f, a, b, N=5):
    """ Multidimensional Gaussian quadrature.
    Here, f is the integrand, a and b are arrays giving the limits
    of the integral, and N is the number of abscissas. """

    a = pylab.asarray(a)
    b = pylab.asarray(b)
    ndim = a.size
    if a.size == 1: # use normal 1d Gaussian quadrature
        return gaussian(f, a, b)
    fac = 0.5*(b-a)
    mid = 0.5*(b+a)
    s = 0.0
    # loop over all possible ndim-vectors of abscissas
    for xw in itertools.product(abscissas(N), repeat=ndim):
        x = pylab.array([x for (x,_) in xw])
        w = pylab.prod([w for (_,w) in xw])
        s += w*f(fac*x+mid)
    return pylab.prod(fac)*s


def montecarlo(f, a, b, eps=1e-3, nmin=100, nmax=1000000):
    """ Monte Carlo integration.
    Here, f is the integrand, a and b are arrays giving the limits
    of the integral, and eps is the desired accuracy.
    The parameters nmin and nmax specify the minimum and
    maximum number of random points to use. """

    a = pylab.asarray(a)
    b = pylab.asarray(b)
    vol = pylab.prod(b-a)
    s = 0.0 # running average of f(x)
    ssq = 0.0 # running sum of (f(x)-s)**2
    n = 0
    while n < nmax:
        n += 1
        x = pylab.uniform(a, b)
        fx = f(x)
        d = fx - s
        s += d/n
        ssq += d*(fx - s)
        err = ssq**0.5/n # assume n-1 ~= n
        if n > nmin and err < eps*abs(s):
            break
    return vol*s
