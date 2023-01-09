from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
a, L, x , EI, psi, kAG, C1, C2, C3, C4, Mo, P = symbols("a L x EI psi kAG C_1 C_2 C_3 C_4 M P")
q = 0
#This seems like the output is incorrect???
EIpsi = integrate(q,x,x,x) + C1*x**2/2 + C2*x + C3 
EIy = integrate(q,x,x,x,x) + C1*x**3/6 + C2*x**2/2 + C3*x + C4 - EI/kAG*(integrate(q,x,x) + C1*x) 
M = EIpsi.diff(x)
V = EIpsi.diff(x,2)
M2 = M.subs(x,L) - Mo
y1 = (EIy/EI).subs(x,0)
psi1 = (EIpsi/EI).subs(x,0)
V2 = V.subs(x,L) - P
s = solve([psi1,y1,M2,V2], [C1, C2, C3, C4])
print(s)
EIpsi = EIpsi.subs({C1:s[C1], C2:s[C2], C3:s[C3], C4:s[C4]})
EIy = EIy.subs({C1:s[C1], C2:s[C2], C3:s[C3], C4:s[C4]})
print("y = ", simplify(EIy/EI))
print("psi = ", simplify(EIpsi/EI))
