""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# Hyperfine.py: Symbolic Hydrogen hyperfine structure using Sympy

from sympy import *
import numpy as np, matplotlib.pyplot as plt

W, mue, mup, B = symbols('W mu_e mu_p B')    # Symbols & Hamiltonian
H  = Matrix([[W,0,0,0],[0,-W,2*W,0],[0,2*W,-W,0],[0,0,0,W]])
Hmag = Matrix([[-(mue+mup)*B,0,0,0],[0,-(mue-mup)*B,0,0],
		[0,0,-(-mue+mup)*B,0],[0,0,0,(mue+mup)*B]])   
           
print "\n Hyperfine Hamiltonian H =", H 
print "\n Eigenvalues and multiplicities of H =",H.eigenvals() 
print "\n Hmag =", Hmag
Htot = H + Hmag                    # Hamiltonian + pertubation
print"\n Htot = H + Hmag =", Htot
print "\n Eigenvalues of matrix HB" 
e1, e2, e3, e4 = Htot.eigenvals()                # 4 eigenvalues
print " e1 = ", e1, "\n e2 = ", e2, "\n e3 = ", e3, "\n e4 = ", e4
print "\n After substitute mu_e = 1, and mu_p = 0 in eigenvalues"
print " e1 = ",e1.subs([(mue,1),(mup,0)]),"\n e2 = ",
e2.subs([(mue,1),(mup,0)])
print " e3 = ",e3.subs([(mue,1),(mup,0)]),"\n e4 = ",
e4.subs([(mue,1),(mup,0)])
b = np.arange(0,4,0.1)
E = 1
E4 = -E + np.sqrt(b**2 +4*E**2)
E3 = E - b
E2 = E + b
E1 = -E - np.sqrt(b**2 +4*E**2)
plt.figure()
plt.plot(b,E1, label='E1');  plt.plot(b,E2, label='E2')
plt.plot(b,E3, label='E3');  plt.plot(b,E4, label='E4')
plt.legend(); plt.text(-0.4,1, 'E') 
plt.xlabel(' Magnetic Field B')
plt.title('Hyperfine Splitting of H Atom 1S Level')
plt.show()