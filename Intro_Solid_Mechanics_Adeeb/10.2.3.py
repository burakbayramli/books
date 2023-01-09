import sympy as sp
from sympy import cos,sin,solve,diff
th = sp.symbols("theta")
Equil = -140-900*cos(th)+750*4*cos(th)*sin(th)
s = solve(Equil,th)
s=[(i.expand(complex=True)).evalf() for i in s]
degs = [(i/sp.pi*180).evalf() for i in s]
print("Equilibrium =",Equil)
print("there are 4 real roots of  the above expression")
print(s)
print("\u03B8_1 =",degs[1])
print("\u03B8_2 =",degs[2])
print("\u03B8_3 =",degs[3])
print("\u03B8_4 =",degs[0]+360)
PE = 750/2*(2*sin(th))**2-140*th-900*sin(th)
DPE = diff(PE,th)
D2PE = diff(DPE,th)
print("Potential Energy =",PE)
print("Derivative of PE",DPE,"Second Derivative of Pe",D2PE)
print("D2PE@\u03B8=\u03B8_1 =",D2PE.subs({th:s[1]}))
print("D2PE@\u03B8=\u03B8_2 =",D2PE.subs({th:s[2]}))
print("D2PE@\u03B8=\u03B8_3 =",D2PE.subs({th:s[3]}))
print("D2PE@\u03B8=\u03B8_4 =",D2PE.subs({th:s[0]+2*sp.pi}))
