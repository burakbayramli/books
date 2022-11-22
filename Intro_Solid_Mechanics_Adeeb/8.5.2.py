from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
x , EI, a, L, Mo, P = symbols("x EI a L M P")
y = Function("y")
th = y(x).diff(x)
M = EI * y(x).diff(x,2)
V = EI * y(x).diff(x,3)
q = -a
M1 = M.subs(x,0)
M2 = M.subs(x,L)
V1 = V.subs(x,0)
V2 = V.subs(x,L)
y1 = y(x).subs(x,0)
y2 = y(x).subs(x,L)
th1 = th.subs(x,0)
th2 = th.subs(x,L)
s = dsolve(EI*y(x).diff(x,4), y(x), ics = {M2/EI:Mo/EI, V2/EI:P/EI, y1:0, th1:0})
display(simplify(s))
