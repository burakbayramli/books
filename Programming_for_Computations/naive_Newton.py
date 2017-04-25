def naive_Newton(f, dfdx, x, eps):
    while abs(f(x)) > eps:
        x = x - float(f(x))/dfdx(x)
        print x
    return x

def f(x):
    return x**2 - 9

def dfdx(x):
    return 2*x

from math import tanh

def f(x):
    return tanh(x)

def dfdx(x):
    return 1 - tanh(x)**2

print naive_Newton(f, dfdx, 1.0887, 0.001)
