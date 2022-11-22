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
display(B,Cc)
KBeforeintegration = simplify(B.transpose()*Cc*B)
display("K before integration: ", KBeforeintegration)
display("Using Exact Integration")
etaset = Matrix([1/sqrt(3), -1/sqrt(3)])
xiset = Matrix([1/sqrt(3), -1/sqrt(3)])
Kfull = 0
for i in range(2):
    for j in range(2):
        Kfull += KBeforeintegration[0].subs({xi:xiset[i], eta:etaset[j]})
display(simplify(Kfull))
Energyfull = us*Kfull*us/2
display(simplify(Energyfull))
display("Using reduced integration")
etaset = xiset = Matrix([0])
Kreduced = 4*KBeforeintegration[0].subs({xi:xiset[0], eta:etaset[0]})
display(Kreduced)
Energyreduced=us*Kreduced*us/2
