def diff(f, x, h, *f_prms):
    print 'x:', x, 'h:', h, 'f_prms:', f_prms
    return (f(x+h, *f_prms) - f(x, *f_prms))/h

def y(t, v0):
    g = 9.81; return v0*t - 0.5*g*t**2

dydt = diff(y, 0.1, 1E-9, 3)
print dydt

from math import *
def G(x, t, A, a, w):
    return A*exp(-a*t)*sin(w*x)

dGdx = diff(G, 0.5, 1E-9, 0, 1, 1.5, 100)
print dGdx

# Keyword arguments

def diff(f, x, h=1E-10, **f_prms):
    print 'x:', x, 'h:', h, 'f_prms:', f_prms
    return (f(x+h, **f_prms) - f(x, **f_prms))/h

def y(t, v0=1):
    g = 9.81; return v0*t - 0.5*g*t**2

dydt = diff(y, 0.1, h=1E-9, v0=3)

def G(x, t=0, A=1, a=1, w=1):
    return A*exp(-a*t)*sin(w*x)

dGdx = diff(G, 0.5, h=1E-9, t=0, A=1, w=100, a=1.5)
dGdx = diff(G, 0.5, t=0, A=1, w=100, a=1.5)
dGdx = diff(G, 0.5, t=0, A=1, w=100, a=1.5, h=1E-9)

# ---

def diff(f, x, h=1E-10, *f_args, **f_kwargs):
    print f_args, f_kwargs
    return (f(x+h, *f_args, **f_kwargs) -
            f(x,   *f_args, **f_kwargs))/h

def y(t, v0):
    g = 9.81; return v0*t - 0.5*g*t**2

dydt = diff(y, 0.1, 1E-9, 3)

def G(x, t, A=1, a=1, w=1):
    return A*exp(-a*t)*sin(w*x)

dGdx = diff(G, 0.5, 1E-9, 0, A=1, w=100, a=1.5)
mycos = diff(sin, 0)


