class Diff:
    def __init__(self, f, h=1E-5):
        self.f = f
        self.h = float(h)

class Forward1(Diff):
    def __call__(self, x):
        f, h = self.f, self.h
        return (f(x+h) - f(x))/h

class Backward1(Diff):
    def __call__(self, x):
        f, h = self.f, self.h
        return (f(x) - f(x-h))/h

class Central2(Diff):
    def __call__(self, x):
        f, h = self.f, self.h
        return (f(x+h) - f(x-h))/(2*h)

class Central4(Diff):
    def __call__(self, x):
        f, h = self.f, self.h
        return (4./3)*(f(x+h)   - f(x-h))  /(2*h) - \
               (1./3)*(f(x+2*h) - f(x-2*h))/(4*h)

class Central6(Diff):
    def __call__(self, x):
        f, h = self.f, self.h
        return (3./2) *(f(x+h)   - f(x-h))  /(2*h) - \
               (3./5) *(f(x+2*h) - f(x-2*h))/(4*h) + \
               (1./10)*(f(x+3*h) - f(x-3*h))/(6*h)

class Forward3(Diff):
    def __call__(self, x):
        f, h = self.f, self.h
        return (-(1./6)*f(x+2*h) + f(x+h) - 0.5*f(x) - \
                (1./3)*f(x-h))/h


def _test():
    mycos = Central4(sin)
    print dir(mycos)
    print mycos.__dict__
    print mycos.__class__.__bases__[0].__dict__
    # Compute sin'(pi)
    print "g'(%g)=%g (exact value is %g)" % (pi, mycos(pi), cos(pi))
    mysin = Central4(Central4(sin))
    # Compute sin''(pi)
    print "g''(%g)=%g (exact value is %g)" % (pi, mysin(pi), -sin(pi))

    df = Central2(lambda x: exp(x), h=1.0E-9)
    bigx = 20
    print exp(bigx), exp(bigx) - df(bigx) 

if __name__ == '__main__':
    from math import *
    _test()


