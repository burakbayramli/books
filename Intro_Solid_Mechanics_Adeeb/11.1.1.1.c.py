from sympy import *
import sympy as sp 
sp.init_printing(use_latex = "mathjax")
x, a1, a2, a3, E, A, P, c = symbols("x a_1 a_2 a_3 E A P c")
u = a3*x**3+a2*x**2+a1*x
uL = u.subs(x,L)
PE = integrate((1/2)*E*A*(u.diff(x)**2), (x,0,L)) - P*uL - integrate(c*x*u, (x,0,L))
display("Potential Energy: ", PE)
Eq1 = PE.diff(a2)
Eq2 = PE.diff(a1)
Eq3 = PE.diff(a3)
display("Minimize PE: ", Eq1, Eq2, Eq3)
s = solve((Eq1, Eq2, Eq3), (a2, a1, a3))
display("Solve: ", s)
u = u.subs({a1:s[a1], a2:s[a2], a3:s[a3]})
display("Best third degree Polynomial(Rayleigh Ritz method): ", u)
stress = E * u.diff(x)
display("stress: ", simplify(stress))
