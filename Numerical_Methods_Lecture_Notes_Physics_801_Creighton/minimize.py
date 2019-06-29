import math, random

goldenratio = 0.5+0.5*5.0**0.5


def bracket(f, a, b):
    """ Brackets the minimum of a function. """

    fa = f(a)
    fb = f(b)
    if fb > fa:  # swap a and b so that f(b) < f(a)
        (a, b) = (b, a)
        (fa, fb) = (fb, fa)
    c = b+(b-a)*goldenratio
    fc = f(c)
    while fb > fc:  # keep going downhill
        (a, b) = (b, c)
        c = b+(b-a)*goldenratio
        (fb, fc) = (fc, f(c))
    return (a, b, c)


def golden(f, a, b, c, eps=1e-6):
    """ Uses a golden section search for the minimum of a function. """

    tolerance = eps**0.5
    fb = f(b)
    while abs(a-c) > tolerance*(abs(a)+abs(c)):
        # make sure that b is closer to c
        if abs(b-a) < abs(c-b):  # swap a with c
            (a, c) = (c, a)
        x = a+(b-a)/goldenratio
        fx = f(x)
        if fx < fb:
            (a, b, c) = (a, x, b)
            fb = fx
        else:
            (a, b, c) = (x, b, c)
    if fx < fb:
        return (x, fx)
    else:
        return (b, fb)


def anneal(f, x0, xnew, T0=None):
    """ Uses simulated annealing to find the minimum of a function. """

    # get number of dimensions of x0, or 1 if scalar
    try: ndim = len(x0)
    except: ndim = 1

    f0 = f(x0)
    T = T0 if T0 is not None else 2.0*f0
    anneal.state = (T, x0, f0) # save state for possible external access

    # loop until no step accepted at current temperature
    accept = True
    while accept:
        accept = False
        for i in range(100*ndim): # steps at this temperature
            x = xnew(x0) # get proposed new position
            fx = f(x)
            if fx < f0 or random.random() < math.exp((f0-fx)/T):
                accept = True # the step has been accepted
                (x0, f0) = (x, fx) # update position
                anneal.state = (T, x0, f0) # update state
        T *= 0.95 # exponential cooling
    return (x0, f0)
