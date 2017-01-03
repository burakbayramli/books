def myfunc(x):
    return x**q

from scipy import integrate
q = 1
v, e = integrate.quad(myfunc, 0, 1)
print v, 1.0/(q+1) - v, e
q = 0.5
v, e = integrate.quad(myfunc, 0, 1)
print v, 1.0/(q+1) - v, e
q = 0.01
v, e = integrate.quad(myfunc, 0, 1)
print v, 1.0/(q+1) - v, e

from scipy.special import jn
def myfunc(x):
    return jn(n,x)

n = 4
v, e = integrate.quad(myfunc, 0, 1)
print v, e
from math import *
print v*exp(-0.02*140)
n = 14
v, e = integrate.quad(myfunc, 0, 1)
print v, e
