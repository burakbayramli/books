""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ODEsympy0.py: Determine params in HO symbolically soltn via sympy 

from sympy import *

x, t,kap,w0,w,alf,x0,p0, m =symbols('x t kap w0 w alf x0 p0 m')
Soltn= exp(alf*t)*(x0*cos(w*t) +p0/(m*w)*sin(w*t)) # Soltn
print "\n Soltn:\n" , Soltn

 # Place in ODE
yp = diff(Soltn,t,t)+kap*diff(Soltn,t)+w0**2*Soltn
print "\n Derivatives:"
print yp

 # Evaluate at t = 0
y2 = yp.subs(t,0)                
print "\n Initial value y2:"
print "y2 = ", y2

# Coefficients of p0/m
eq1 = 2*alf+kap   
A = solveset(eq1,alf)  
print "\n Coefficients of p0/m, A = ", A 

# Coefs x0
eq2 = alf**2+kap*alf -w**2 +w0**2 
W = simplify(solveset(eq2,w))
print "\n W = ",W 
freq=W.subs(alf,-kap/2)  # substitute kalf with -kap/2
print "\n frequency w = ",simplify(freq)  # simplify