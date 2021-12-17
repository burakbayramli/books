from __future__ import print_function

from scipy.special import erf
import math

# compute erf(1) by numerically doing the integral as a 3-point quadrature:
# trapezoid (compound), Simpson's, and Gauss-Legendre
#
# erf(x) = 2/sqrt(pi) int_0^x exp(-y^2) dy
#


# analytic integral
def analytic():
    return erf(1)

def f(y):
    """ integrand """
    return (2.0/math.sqrt(math.pi))*math.exp(-y**2)


def x(z, a, b):
    """ convert from [-1, 1] (the integration range of Gauss-Legendre)
        to [a, b] (our general range) through a change of variables z
        -> x """

    return 0.5*(b + a) + 0.5*(b - a)*z


def main():

    # integration limits
    a = 0.0
    b = 1.0

    # we are doing 3-point quadrature for all methods.  delta is the
    # width of the slab
    delta = 0.5

    # trapezoidal
    trap = 0.5*delta*(f(a) + f(0.5*(a+b))) + 0.5*delta*(f(0.5*(a+b)) + f(b))

    # Simpson's
    simp = (delta/3.0)*(f(a) + 4.0*f(0.5*(a+b)) + f(b))

    # Gauss-Legendre

    # we need to convert from [-1, 1] (the range in which the roots
    # are found) to [a, b] (the range in which our integrand is
    # defined), so convert the roots z1, z2, and z3

    z1 = -math.sqrt(3./5.)
    x1 = x(z1, a, b)
    w1 = 5./9.

    z2 = 0
    x2 = x(z2, a, b)
    w2 = 8./9.

    z3 = math.sqrt(3./5.)
    x3 = x(z3, a, b)
    w3 = 5./9.

    # 3-point Gauss-Legendre quadrature -- note the factor in the front
    # is a result of the change of variables from x -> z
    integral = 0.5*(b-a)*(w1*f(x1) + w2*f(x2) + w3*f(x3))

    print("erf(1) (exact):         ", analytic())
    print("3-point trapezoidal:    ", trap, trap-analytic())
    print("3-point Simpson's:      ", simp, simp-analytic())
    print("3-point Gauss-Legendre: ", integral, integral-analytic())


if __name__ == "__main__":
    main()
