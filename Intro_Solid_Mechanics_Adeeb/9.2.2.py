import sympy as sp
import numpy as np
from sympy import Matrix, simplify, diff, eye, det
from matplotlib import pyplot as plt
theta = sp.symbols("theta")
Q = Matrix([[sp.cos(theta), sp.sin(theta),0],
            [-sp.sin(theta),sp.cos(theta), 0],
            [0,0,1]])
Q = Q.subs({theta:sp.pi/2})
print("Q =", Q)
X1,X2,X3 = sp.symbols("X_1 X_2 X_3")
a,b,c = sp.symbols("a b c")
ea,eb,ec,t = sp.symbols("epsilon_a epsilon_b epsilon_c t")
f0 = sp.symbols("f_0")
xi1 = (1+t*ea)*X1
xi2 = (1-t*eb)*X2
xi3 = (1-t*ec)*X3
xi = Matrix([xi1, xi2, xi3])
x = Q*xi
X = Matrix([X1,X2,X3])
U = Matrix([[diff(i,j) for j in X] for i in xi])
print("Matrix U appearing in the Right polar decomposition of the deformation gradient F")
print("U =", U)
Udot = diff(U,t)
print("Udot =",Udot)
F = Matrix([[diff(i,j) for j in X] for i in x])
print("Deformation gradient F =",F)
V = F*Q.T
print("Matrix V appearing in the Right polar decomposition of the deformation gradient  F")
print("V =",V)
Fdot = diff(F,t)
print("Fdot =",Fdot)
L = Fdot*F.inv()
print("L =",L)
print("L @t=0.5 = ", L.subs({t:0.5}))
print("L @t=1 = ", L.subs({t:1}))
Dd = 1/2*(L+L.T)
print("Symmetric part of L", Dd)
Elagrange = 1/2*(F.T*F-eye(3))
print("Green Strain =",Elagrange)
Elagrangedot = diff(Elagrange,t)
print("Green Strain Rate =",Elagrangedot)
sigmalocal = Matrix([[t*f0/(b*c*(1-t*eb)*(1-t*ec)),0,0],[0,0,0],[0,0,0]])
Cauchy = Q*sigmalocal*Q.T
print("Cauchy stress =",Cauchy)
print("Cauchy stress @t=0:",Cauchy.subs({t:0}))
print("Cauchy stress @t=0.5:",Cauchy.subs({t:0.5}))
print("Cauchy stress @t=1:",Cauchy.subs({t:1}))
P = simplify(det(F)*Cauchy*F.T.inv())
print("P =",P)
print("P @ t=0",P.subs({t:0}))
print("P @ t=0.5",P.subs({t:0.5}))
print("P @ t=1",P.subs({t:1}))
S = F.inv()*P
print("S =",S)
print("S @ t=0",S.subs({t:0}))
print("S @ t=0.5",S.subs({t:0.5}))
print("S @ t=1",S.subs({t:1}))
# code is the same below
e1 = sum([sum([Cauchy[i,j]*Dd[i,j] for i in range(3)]) for j in range(3)])
#e1 = sum([Cauchy[i]*Dd[i] for i in range(9)])
e1 = sum([Cauchy[i]*Dd[i] for i in range(9)])
print("Cauchy",Cauchy,"Dd",Dd)
print("D and Cauchy are energy conjugates: dU/dt", e1)
e2 = sum([P[i]*Fdot[i] for i in range(9)])
print("P",P,"Fdot",Fdot)
print("P and Fdot are energy conjugates: dW/dt", e2)
e2 = sum([S[i]*Elagrangedot[i] for i in range(9)])
print("S",S,"E_Green",Elagrangedot)
print("S and E_Green are energy conjugates: dW/dt", e2)
