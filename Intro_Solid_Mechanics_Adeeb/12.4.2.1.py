from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
x1, x2, w1, w2 = sp.symbols("x_1 x_2 w_1 w_2")
#Sympy has a library for Gauss integration
Eq1 = w1+w2-2
Eq2 = w1*x1**3+w2*x2**3
Eq3 = w1*x1**2+w2*x2**2-2/3
Eq4 = w1*x1+w2*x2
s = solve((Eq1,Eq2,Eq3,Eq4), (x1,x2,w1,w2))
print(s)
