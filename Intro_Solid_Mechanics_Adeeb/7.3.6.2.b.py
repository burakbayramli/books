import sympy as sp
from sympy import *
F = Matrix([[1/4,-1/(2*sp.sqrt(2)),1/4],
           [1/4,1/(2*sp.sqrt(2)),1/4],
           [-2*sp.sqrt(2),0,2*sp.sqrt(2)]])
J = det(F)
mu = 1
p = sp.symbols("p")
display("J = det(F) =",J)
display("F =",F)
I = sum(F*F.T)
display("I = sum(FF^T)",I)
P = 4*mu*F+J*p*F.inv().T
display("P = 4\u03BCF + JpF^(-T) =",P)
s = 4*mu*F*F.T+p*eye(3)
display("\u03C3 = 4\u03BCFFF^T+pI =",s)
