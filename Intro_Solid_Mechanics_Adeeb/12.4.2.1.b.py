from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
xi, eta, nu, E, us = sp.symbols("xi eta nu E u_s")
N1=(1-xi)*(1-eta)/4
N2=(1+xi)*(1-eta)/4
N3=(1+xi)*(1+eta)/4
N4=(1-xi)*(1+eta)/4
B = Matrix([(N1-N2+N3-N4).diff(xi),0,(N1-N2+N3-N4).diff(eta)])
Cc = E/(1+nu)/(1-nu)*Matrix([[1,nu,0],[nu,1,0],[0,0,(1-nu)/2]])
print(B,Cc)
KBeforeintegration = simplify(B.transpose()*Cc*B)
print("K before integration: ", KBeforeintegration)
print("Using Exact Integration")
etaset = Matrix([1/sqrt(3), -1/sqrt(3)])
xiset = Matrix([1/sqrt(3), -1/sqrt(3)])
Kfull = 0
for i in range(2):
    for j in range(2):
        Kfull += KBeforeintegration[0].subs({xi:xiset[i], eta:etaset[j]})
print(simplify(Kfull))
Energyfull = us*Kfull*us/2
print(simplify(Energyfull))
print("Using reduced integration")
etaset = xiset = Matrix([0])
Kreduced = 4*KBeforeintegration[0].subs({xi:xiset[0], eta:etaset[0]})
print(Kreduced)
Energyreduced=us*Kreduced*us/2
