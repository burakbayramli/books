from sympy import *
import sympy as sp 
sp.init_printing(use_latex = "mathjax")
x, A, E , C, L, P = symbols("x A E C L P")
u = Function("u")
u1 = u(x).subs(x,0)
u2 = u(x).diff(x).subs(x,L)
a = dsolve(u(x).diff(x,2) + C*x/(E*A), u(x), ics = {u1:0, u2:P/(E*A)})
u = a.rhs
sigma = E * u.diff(x)
print("displacement and stress: ", u, simplify(sigma))
