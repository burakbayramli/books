import sympy as sp
from sympy import *
F = Matrix([[1/4,-1/(2*sp.sqrt(2)),1/4],
           [1/4,1/(2*sp.sqrt(2)),1/4],
           [-2*sp.sqrt(2),0,2*sp.sqrt(2)]])
J = det(F)
mu = 1
p = sp.symbols("p")
print("J = det(F) =",J)
print("F =",F)
I = sum(F*F.T)
print("I = sum(FF^T)",I)
P = 4*mu*F+J*p*F.inv().T
print("P = 4\u03BCF + JpF^(-T) =",P)
s = 4*mu*F*F.T+p*eye(3)
print("\u03C3 = 4\u03BCFFF^T+pI =",s)
