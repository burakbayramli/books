# use bisection, Newton's method, or secant method to find roots
#
# M. Zingale (2013-02-14)

import math


# sample equations to find the roots of
def f(x):
    return (x - 1.0)**2 - 4

def fprime(x):
    return 2.0*(x - 1.0)


def g(x):
    return math.exp(x) - 4.0

def gprime(x):
    return math.exp(x)


def h(x):
    return x*x   # bisection can't do this...

def hprime(x):
    return 2.0*x


class root:
    """ simple class to manage root finding.  All method take in a
        function and desired tolerance """

    def __init__(self, fun, tol, fprime=None):
        self.f = fun
        self.fprime = fprime

        self.tol = tol


    def bisection(self, xl, xr):
        """ find the root using bisection.  xl and xr should bracket
            the root """

        # initial evaluations
        fl = self.f(xl)
        fr = self.f(xr)
        
        # do we contain a single (odd number actually...) root?
        # (yes if fl and fr have different signs)
        if fl*fr >= 0:
            return None

        err = abs(xr - xl)
        while (err > self.tol):
            xm = 0.5*(xl + xr)
            fm = self.f(xm)

            if fm*fl >= 0:
                # root is in the right half
                xl = xm
                fl = fm

            else:
                # root is in the left half
                xr = xm
                fr = fm

            err = abs(xr - xl)
            print xm, err

        return 0.5*(xl + xm)


    def newton(self, x0):
        """ find the root via Newton's method.  x0 is the initial guess
            for the root """

        # initial change
        dx = -self.f(x0)/self.fprime(x0)
        x = x0 + dx
    
        while (abs(dx) > self.tol):
            print x, dx
            dx = -self.f(x)/self.fprime(x)            
            x += dx

        return x


    def secant(self, xm1, x0):
        """ find the root via Newton's method.  xm1 and x0 are two
            initial guesses close to the root"""

        # initial change
        dx = -self.f(x0)*(x0 - xm1)/(self.f(x0) - self.f(xm1))
        xm1 = x0
        x = x0 + dx

        # loop.  xm1 will always carry the previous estimate for the
        # root
        while (abs(dx) > self.tol):
            print x, dx
            dx = -self.f(x)*(x - xm1)/(self.f(x) - self.f(xm1))
            xm1 = x
            x += dx


        return x


r = root(f, 1.e-6, fprime=fprime)
print r.bisection(0.0, 10.0)
print " "

print r.newton(10.0)
print " "

print r.secant(10.0, 9.0)


                
