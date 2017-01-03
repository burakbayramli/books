#!/usr/bin/env python
from numpy import linspace

def Trapezoidal_vec(a, b, f, n):
    h = (b-a)/float(n)
    x = linspace(a, b, n+1)
    v = f(x)
    r = h*(sum(v) - 0.5*(v[0] + v[-1]))
    return r

def Trapezoidal2_vec(a, b, f, n):
    """alternative to Trapezoidal"""
    h = (b-a)/float(n)
    x = linspace(a, b, n+1)
    r = h*f(x)
    r[0] /= 2.0
    r[-1] /= 2.0
    return sum(r)

# functions to be integrated:
def f1(x):
    f = exp(-x*x)*log(1+x*sin(x))
    return f

def f1loop(x):
    """as f1 but explicit loop instead of NumPy math functions"""
    f = zeros(len(x))
    for i in range(len(x)):
        f[i] = f1(x[i])
    return f

def f2(x):
    return 1 + x

a = 0; b = 2; n = 1000
for i in range(10000):
    result = Trapezoidal_vec(a, b, f1, n)
import time
t1 = time.clock()
print result, t1

# f1loop ran 43 times slower than f1 on IBM X30
