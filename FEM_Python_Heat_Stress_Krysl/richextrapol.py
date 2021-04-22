# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017-2018, Petr Krysl
"""
Richardson extrapolation.
"""

import math
from numpy import diff

def bisection(fun, xl, xu, tolx, tolf):
    if (xl > xu):
        xl, xu = xu, xl
    fl = fun(xl);
    fu = fun(xu);
    while True:
        xr = (xu + xl) * 0.5  # bisect interval
        fr = fun(xr)  # value at the midpoint
        if fr * fl < 0.0: # (fr < 0.0 && fl > 0.0) || (fr > 0.0 && fl < 0.0)
            xu, fu = xr, fr # upper --> midpoint
        else:
            xl, fl = xr, fr # lower --> midpoint
        if (abs(xu-xl) < tolx) or (abs(fr) <= tolf):
            return xl, xu # We are done
    return xl, xu

def richextrapol(xs, hs):
    """
    Richardson extrapolation.

     xestim, beta, c, residual = richextrapol(xs,hs)

    Richardson extrapolation. This function is applicable to fixed ratio
    between the element sizes, hs[0]/hs[1] == hs[1]/hs[2], in which case
    the solution follows explicitly, But it is also applicable to non-uniform
    refinement factor (for arbitrary element sizes).

    Arguments
         xs = list of the calculated quantities,
         hs = list of the element sizes

    Returns
        xestim= estimate of the asymptotic solution from the data points
            in the xs array
        beta= convergence rate
        c = constant in the estimate "error = c*h**beta"
        residual = residual after equations from which the above quantities were
            solved (this is a measure of how accurately was the system solved)
    """
    # Normalize the solutions so that we work with nice numbers
    nxs = [x/xs[0] for x in xs]
    c, beta = 0.0, 1.0 # some defaults to be overwritten below
    if abs(hs[0]/hs[1] - hs[1]/hs[2]) > 1.0e-6:
        # In the hard case, with a non-uniform refinement factor, we must
        #        solve a nonlinear equation 1st.
        nea1 = nxs[1]-nxs[0]
        nea2 = nxs[2]-nxs[1]
        tol = 0.000000001
        h0, h1, h2 = hs[0], hs[1], hs[2]
        eqn = lambda b: nea1 - nea2 * ((-h1**b+h0**b) / (-h2**b+h1**b))
        lo = tol # lower bound on the convergence rate
        hi = 10.0
        b1, b2 = bisection(eqn, lo, hi, tol, tol)
        beta = (b1 + b2) / 2.0
        c = xs[0]*nea1/(-hs[1]**beta+hs[0]**beta)
        xestim = xs[2] + c * hs[2]**beta
    else:
        # In the easy case, with a uniform refinement factor, the solution
        #        is obtained explicitly.
        xestim = ((-(nxs[0]*nxs[2]-nxs[1]**2)/(2*nxs[1]-nxs[0]-nxs[2])))*xs[0]
        if (xestim-xs[0]) <= 0:
            beta = math.log((xestim-xs[1])/(xestim-xs[2]))/math.log(hs[1]/hs[2])
        else:
            beta = math.log((xestim-xs[0])/(xestim-xs[2]))/math.log(hs[0]/hs[2])
        c = (xestim-xs[0])/hs[0]**beta

    # just to check things, calculate the residual
    residual = [0, 0, 0]
    for I in range(0, 3):
        residual[I] = (xestim-xs[I])-c*hs[I]**beta# this should be close to zero
    #
    return xestim, beta, c, residual

def _test():
    """
    Test of Richardson extrapolation.
    """
    xs = [93.0734, 92.8633, 92.7252]
    hs = [0.1000, 0.0500, 0.0250]
    xestim, beta, c, residual = richextrapol(xs, hs)
    print('xestim, beta, c, residual =', xestim, beta, c, residual)
    print('to be compared with ', 92.46031652777476, 0.6053628424093497, -2.471055221256022, \
          [0.0, 5.534461777756405e-13, 3.6376457401843254e-13])

    xs = [13.124299546191557, 12.192464513175167, 12.026065694522512]
    hs = [1.0, 0.5, 0.25]
    xestim, beta, c, residual = richextrapol(xs, hs)
    print('xestim, beta, c, residual =', xestim, beta, c, residual)
    print('to be compared with ', 11.989892116202842, 2.485429379525128, -1.1344074299887144, \
          [0.0, -8.93729534823251e-15, -1.5959455978986625e-15])

    xs = [18.279591331058882, 18.260877354949294, 18.255333231883284]
    hs = [0.25, 0.125, 0.0625]
    xestim, beta, c, residual = richextrapol(xs, hs)
    print('xestim, beta, c, residual =', xestim, beta, c, residual)
    print('to be compared with ', 18.25299931817398, 1.7550849296401745, -0.3029825595040225, \
          [0.0, -3.975153539670373e-13, -1.1776647365624449e-13])

    sol = [231.7, 239.1, 244.8]
    h = [(4./5.)**i for i in range(0, 3)]
    xestim, beta, c, residual = richextrapol(sol, h)
    print('xestim, beta, c =', xestim, beta, c)
    print('to be compared with ', 263.91176470588067, 1.1697126080157385, 32.21176470588068)

    # This Tests the case of non-uniform refinement factor
    sol = [7.446, 7.291, 6.87] # f = lambda h: -3.333 * h**1.2 + 6.66
    h = [0.3, 0.25, 0.1]
    xestim, beta, c, residual = richextrapol(sol, h)
    print('xestim, beta, c =', xestim, beta, c)
    print('to be compared with ', 6.66, 1.2, -3.333)

_test()

def examples():
    """
    Examples from the textbook.
    """
    sol = [231.7, 239.1, 244.8]
    h = [(4./5.)**i for i in range(0, 3)]
    xestim, beta, c, residual = richextrapol(sol, h)
    print('xestim, beta, c =', xestim, beta, c)
    #
    Ea = diff(sol)
    print(Ea)
    # Plotting
    import matplotlib.pyplot as plt
    plt.figure()
    plt.xlim((0.5, 1.1))
    plt.ylim((0.005, 0.2))
    # Normalized true error estimate
    nEt = [(xestim-s)/xestim for s in sol]
    plt.loglog(h, nEt, 'k-s', linewidth=3)
    plt.loglog(h[1:3], Ea/max(sol), 'r--+', linewidth=3)

    plt.grid()
    plt.title('Estimated errors')
    plt.xlabel('Refinement factor (ND)')
    plt.ylabel('Normalized error (ND)')
    plt.show()

    from math import log
    print(sol[1]+(sol[2]-sol[1])/(1-(4/5)**1.0))
    print((log(Ea[1])-log(Ea[0]))/(log(h[2])-log(h[1])))
    print(sol[1]+(sol[2]-sol[1])/(1-(4/5)**1.1697))

#examples()
