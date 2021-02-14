""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Plm.py: Associated Legendre Polynomials via Integration

import numpy as np
import matplotlib.pylab as plt  
from rk4Algor import rk4Algor

xx = np.zeros((1999),float);  yy = np.zeros((1999),float)  # xx=cos, yy=Plm
y = [0]*(2);   dx = 0.001                # Solution array diension, stepsize 
  
def f(x,y):                          # RHS of equation
    rhs = [0]*(2)                    # Declare array dimension.
    rhs[0] = y[1]
    rhs[1] = 2*x*y[1]/(1-x**2)-(el*(el+1)-m**2/(1-x**2))*y[0]/(1-x**2)
    return rhs

el = 4;  m = 2                            # m intger  m<=el,   m = 1,2,3,...
if el == 0 or el == 2:
    y[0] = 1
if( el>2 and (el)%2  ==  0):      
    if m  ==  0:
        y[0] = -1
    elif( m>0 ) :
        y[0] = 1
    elif m<0 and abs(m)%2 == 0:
        y[0] = 1
    elif m<0 and abs(m)%2 == 1:
        y[0] = -1
if el>2 and el%2 == 1 :
    if m == 0:
       y[0] = 1
    elif m>0:
        y[0] = -1
    elif m<0:
        y[0] = 1
y[1] = 1

f(0,y)     # call function for xi = 0 with init conds.
i = -1
for x in np.arange(-0.999999, 1-dx, dx):
    i = i+1
    xx[i] = x
    y = rk4Algor(x, dx, 2, y, f)  # call runge kutt
    yy[i] =  y[0]   #

plt.figure()
plt.plot(xx,yy)
plt.grid()
plt.title('Unormalized $\mathbf{P_l^m(x)}$')
plt.xlabel('x = cos(theta)')
plt.ylabel('$\mathbf{P_l^m(x)}$')
plt.show()