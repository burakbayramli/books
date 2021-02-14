""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Plm.py: Associated Legendre Polynomials via Integration

import numpy as np, matplotlib.pylab as plt  
from rk4Algor import rk4Algor

CosTheta = np.zeros((1999),float)
Plm = np.zeros((1999),float)  
y = [0]*(2);   dCos = 0.001   
el = 4;  m = 2   # m intger  m<=el,   m = 1,2,3,...
if el == 0 or el == 2: y[0] = 1
if (el>2 and (el)%2  ==  0):      
    if m  ==  0: y[0] = -1
    elif( m>0 ): y[0] = 1
    elif m<0 and abs(m)%2 == 0: y[0] = 1
    elif m<0 and abs(m)%2 == 1: y[0] = -1
if (el>2 and el%2 == 1) :
    if m == 0: y[0] = 1
    elif m>0:  y[0] = -1
    elif m<0:  y[0] = 1
y[1] = 1

def f(Cos, y):                           # RHS of equation
    rhs = [0]*(2)                # Declare array dimension
    rhs[0] = y[1]
    rhs[1] = 2*Cos*y[1]/(1-Cos**2)-(el*(el+1)
    	    -m**2/(1-Cos**2))*y[0]/(1-Cos**2)
    return rhs

f(0,y)     # Call function for xi = 0 with init conds.
i = -1
for Cos in np.arange(-0.999999, 1-dCos, dCos):
    i = i+1
    CosTheta[i] = Cos
    y = rk4Algor(Cos, dCos, 2, y, f)  # call runge kutt
    Plm[i] =  y[0]   #

plt.figure()
plt.plot(CosTheta,Plm)
plt.grid()
plt.title('Unormalized $\mathbf{P_l^m(Cos)}$')
plt.xlabel('cos(theta)')
plt.ylabel('$\mathbf{P_l^m(Cos)}$')
plt.show()