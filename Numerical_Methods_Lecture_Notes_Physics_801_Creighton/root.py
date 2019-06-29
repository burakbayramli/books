def bisect(f, a, b, eps=1e-6):
    """ Finds the root of function f in the interval (a,b) by bisection. """

    # orient the search so that f(a) < 0 and f(b) > 0
    if f(a) > 0:  # swap a and b
        (a, b) = (b, a)
    while abs(a-b) > eps:
        xmid = (a+b)/2.0
        if f(xmid) < 0:
            a = xmid
        else:
            b = xmid
    return xmid


def newton(f, x, dfdx=None, eps=1e-6):
    """ Finds the root of a function using Newton's method. """

    if dfdx is None:  # for estimating derivative
        delta = eps**0.5
    while True:
        fx = f(x)
        if dfdx is None:
            dx = delta*x
            if abs(dx) < delta:
                dx = delta
            df = (f(x+dx)-fx)/dx
        else:
            df = dfdx(x)
        dx = -fx/df
        x += dx
        if abs(dx) < eps:
            return x
