from sympy import *
import sympy as sp 
sp.init_printing(use_latex = "mathjax")
x, a1, a2, E, A, P, c, L = symbols("x a_1 a_2 E A P c L")
u = a2*x**2+a1*x
uL = u.subs(x,L)
PE = integrate((1/2)*E*A*(u.diff(x)**2), (x,0,L)) - P*uL - integrate(c*x*u, (x,0,L))
print("Potential Energy: ", PE)
Eq1 = PE.diff(a2)
Eq2 = PE.diff(a1)
print("Minimize PE: ", Eq1, Eq2)
s = solve((Eq1, Eq2), (a2, a1))
print("Solve: ", s)
u = u.subs({a1:s[a1], a2:s[a2]})
print("Best second degree Polynomial(Rayleigh Ritz method): ", u)
stress = E * u.diff(x)
print("stress: ", simplify(stress))
