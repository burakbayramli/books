from sympy import *

x = Symbol('x')
y = Symbol('y')

print 2*x + 3*x - y                # Algebraic computation
print diff(x**2, x)                # Differentiates x**2 wrt. x
print integrate(cos(x), x)         # Integrates cos(x) wrt. x
print simplify((x**2 + x**3)/x**2) # Simplifies expression
print limit(sin(x)/x, x, 0)        # Finds limit of sin(x)/x as x->0
print solve(5*x - 15, x)           # Solves 5*x = 15
