from sympy import Matrix
import sympy as sp
sp.init_printing(use_latex="mathjax")
display("Units are in MPa")
P = 3
d = 2000
t = 10
r = d/2
def vonMises(M):
    return sp.sqrt(1/2*((M[0,0] - M[1,1])**2+(M[1,1] - M[2,2])**2+
                       (M[2,2] - M[0,0])**2+6*(M[0,1]**2+M[0,2]**2+M[1,2]**2)))
s_11 = (P*sp.pi*r**2)/(2*sp.pi*r*t)
display("\u03C3_11 =",s_11)
s_22 = P*r/t
display("\u03C3_22 =",s_22)
s = Matrix([[s_11,0,0],[0,s_22,0],[0,0,0]])
display("\u03C3 =",s)
display("Von Mises =",vonMises(s))
