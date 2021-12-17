import math
import numpy as np

# perform 3-point Gauss-Legendre quadrature on a degree 5 (2 N - 1)
# polynomial, and compare to the compound Simpson's rule.

degree = 5

# integrand
def f(x):
    """ create a polynomial of degree 'degree' """
    p = 0.0
    for n in range(degree+1):
        p += x**n

    return p


# analytic (true) integral
def true(a, b):
    t = 0.0
    for n in range(degree+1):
        t += (b**(n+1) - a**(n+1))/(n+1)

    return t


# do a Simpson's integration by breaking up the domain [a,b] into N
# slabs.  Note: N must be even, because we do a pair at a time
def simp(a,b,f,N):

    xedge = np.linspace(a,b,N+1)

    integral = 0.0

    if not N%2 == 0:
        sys.exit("ERROR: N must be even")

    delta = (xedge[1] - xedge[0])

    for n in range(0, N, 2):
        integral += (1.0/3.0)*delta*(f(xedge[n]) +
                                     4.0*f(xedge[n+1]) +
                                     f(xedge[n+2]))
    return integral


def x(z, a, b):
    """ convert from [-1, 1] (the integration range of Gauss-Legendre)
        to [a, b] (our general range) through a change of variables z
        -> x """

    return 0.5*(b + a) + 0.5*(b - a)*z


# integration limits
a = 0.0
b = 1.0

# Simpson's
I_S = simp(a, b, f, 2)


# Gauss-Legendre

# the roots and corresponding weights
zs = [-math.sqrt(3./5.), 0.0, math.sqrt(3./5.)]
ws = [5./9., 8./9., 5./9.]

integral = 0.0
for z, w in zip(zs, ws):
    # we need to evaluate f() at x(z), since our roots are in z:
    # [-1,1], but our function is in x: [a, b]
    integral +=  w*f(x(z, a, b))

# this factor results from the [-1,1] to [a,b] change
integral *= 0.5*(b-a)

print("exact:                  ", true(a,b))
print("3-point Simpson's:      ", I_S, I_S-true(a,b))
print("3-point Gauss-Legendre: ", integral, integral-true(a,b))

