from sympy import *
import sympy as sp 
sp.init_printing(use_latex = "mathjax")
u, c, EA, x, L = symbols("u c EA x L")
u = Function("u")
u1 = u(x).subs(x,0)
u2 = u(x).diff(x).subs(x,L)
sol = dsolve(u(x).diff(x,2)+c*x/EA, u(x), ics = {u1:0, u2:0})
display(sol)
