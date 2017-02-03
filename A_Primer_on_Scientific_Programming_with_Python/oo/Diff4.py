"""
Functional programming version of Diff.py with the same
interface for the end user.
"""
def differentiate(f, method, h=1E-9):
    h = float(h)  # avoid integer division

    if method == 'Forward1':
        def Forward1(x):
            return (f(x+h) - f(x))/h
	return Forward1

    elif method == 'Backward1':
        def Backward1(x):
            return (f(x) - f(x-h))/h
	return Backward1

    elif method == 'Central2':
        def Central2(x):
            return (f(x+h) - f(x-h))/(2*h)
	return Central2

    elif method == 'Central4':
        def Central4(x):
            return (4./3)*(f(x+h)   - f(x-h))  /(2*h) - \
                   (1./3)*(f(x+2*h) - f(x-2*h))/(4*h)
	return Central4

    elif method == 'Central6':
        def Central6(x):
            return (3./2) *(f(x+h)   - f(x-h))  /(2*h) - \
                   (3./5) *(f(x+2*h) - f(x-2*h))/(4*h) + \
                   (1./10)*(f(x+3*h) - f(x-3*h))/(6*h)
	return Central6

    elif method == 'Forward3':
        def Forward3(x):
            return (-(1./6)*f(x+2*h) - f(x+h) - 0.5*f(x) - \
                    (1./3)*f(x-h))/h
	return Forward3

def _test():
    mycos = differentiate(sin, 'Central4')
    print dir(mycos)
    print mycos.__dict__
    print mycos.__class__.__bases__[0].__dict__
    # Compute sin'(pi)
    print "g'(%g)=%g (exact value is %g)" % (pi, mycos(pi), cos(pi))
    mmysin = differentiate(mycos, 'Central4')
    # Compute sin''(pi)
    print "g''(%g)=%g (exact value is %g)" % (pi, mmysin(pi), -sin(pi))

    df = differentiate(lambda x: exp(x), 'Central2')
    bigx = 20
    print exp(bigx), exp(bigx) - df(bigx) 

if __name__ == '__main__':
    from math import *
    _test()
