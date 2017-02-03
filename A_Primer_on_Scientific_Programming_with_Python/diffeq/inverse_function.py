"""
Given a set of coordinates in the array xcoor, and a Python function
f(x), compute the inverse of f(x), call it g(x), and return the array
g(xcoor). The computation is done point by point. We have

    f(g(x)) = x

which holds at every point xi:

    f(g(xi)) = xi

We do not know the value g(xi) so let us call it gamma. Then
we have the (generally) nonlinear equation

    f(gamma) = xi

for gamma, which can be solved by, e.g., Newton's method.
"""

from Newton import Newton
from scitools.std import *

def f(x):
    return x**2 - 1
    #return x**2 - 1

def F(gamma):
    return f(gamma) - xi

def dFdx(gamma):
    return (F(gamma+h) - F(gamma-h))/(2*h)

h = 1E-6
x = linspace(0.01, 3, 21)
g = zeros(len(x))

for i in range(len(x)):
    xi = x[i]

    # Compute start value (use last g[i-1] if possible)
    if i == 0:
        gamma0 = x[0]
    else:
        gamma0 = g[i-1]

    gamma, n, F_value = Newton(F, gamma0, dFdx)
    g[i] = gamma

plot(x, f(x), 'r-', x, g, 'b-', 
     title='f1', legend=('original', 'inverse'),
     hardcopy='tmp.eps')



        
            
    
