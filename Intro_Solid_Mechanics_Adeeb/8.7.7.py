from sympy import *
from numpy import *
import sympy as sp
import numpy as np
sp.init_printing(use_latex = "mathjax")
L, x , A, Al, ro, E, u, pi, P= symbols("L x A Al rho E u pi P")
u = Function("u")
u1 = u(x).subs(x,0)
u2 = u(x).diff(x).subs(x,L)
A = pi*(2-x/L)**2
print("Area: ", A)
Al = A.subs(x,L)
s = dsolve((E*A*u(x).diff(x)).diff(x) + ro * A, u(x), ics = {u1:0, u2:P/E/Al})
print(s.subs(L, 4))
