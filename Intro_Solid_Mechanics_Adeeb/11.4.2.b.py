from sympy import *
import sympy as sp 
sp.init_printing(use_latex = "mathjax")
w, c, EA, x, L, a1, a2, a3 = symbols("w c EA x L a_1 a_2 a_3")
w1 = x
w2 = x**2
w3 = x**3
u = a1*x+a2*x**2+a3*x**3
Eq1 = integrate(w1.diff(x)*u.diff(x), (x,0,L)) - integrate(w1*c*x/EA, (x,0,L))
Eq2 = integrate(w2.diff(x)*u.diff(x), (x,0,L)) - integrate(w2*c*x/EA, (x,0,L))
Eq3 = integrate(w3.diff(x)*u.diff(x), (x,0,L)) - integrate(w3*c*x/EA, (x,0,L))
s = solve((Eq1,Eq2,Eq3), (a1,a2,a3))
u = u.subs({a1:s[a1],a2:s[a2],a3:s[a3]})
print(u)
