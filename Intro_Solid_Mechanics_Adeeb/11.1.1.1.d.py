from sympy import *
import sympy as sp 
sp.init_printing(use_latex = "mathjax")
x, A, E , L, P = symbols("x A E L P")
u = Function("u")
u1 = u(x).subs(x,0)
u2 = u(x).diff(x).subs(x,L)
Ax = 25/100*(5/10-25/200*x)
Ax1 = Ax.subs(x,L)
print("Area: ", Ax)
s = dsolve(E*u(x).diff(x,2)*Ax+E*u(x).diff(x)*Ax.diff(x), u(x), ics = {u1:0, u2:200/E/Ax1})
u = s.rhs.subs({E:100000, L:2, P:200})
stress = (E * u.diff(x)).subs({E:100000, L:2, P:200})
print("displacement and stress: ", u, stress)
