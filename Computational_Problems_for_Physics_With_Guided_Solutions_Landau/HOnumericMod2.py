""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# HOnumeric.py: Quantum HO wave functions via ODE solver

import numpy as np
import matplotlib.pylab as plt
from rk4Algor import rk4Algor                                   

n = 5   #  Quantum number n = npr + L + 1 = integer > 0                    
xx = np.zeros((1000),float)            # x values for plot
yy = np.zeros((1000),float)            # wave function values
fvector = [0]*(2)                      # force function f
y = [0]*(2)     # array for 2 values

def f(x,y):                          # Force function for HO                     
    fvector[0] = y[1]
    fvector[1] = -(2*n+1-x**2)*y[0]
    return fvector
   
if (n%2 == 0):   y[0] = 1.;   y[1] = 0.   # Even parity
else:            y[0] = 0;    y[1] = -1.   # Odd parity     

xRight = 5
f(0,y)     # force function at starting value
dx = 0.01
i = 0

# Compute WF from 0 to xRight in steps of dr
for x in np.arange(0,xRight,dx):    
    xx[i+500] = x
    y = rk4Algor(x, dr, 2, y, f) 
    yy[i+500] = y[0]   #  
    i = i+1      # Advance i as well as x
    
i = 0
for x in np.arange(-dx,-xRight,-dx):    
    xx[499-i] =  -xx[i+499]
    if (n%2 == 0): yy[499-i] = yy[i+499]    # Even parity
    else:          yy[499-i] = -yy[i+499]    # Ddd parity
    i = i + 1
plt.figure()
plt.plot(xx,yy) 
plt.grid()
plt.title('Harmonic Oscillator Wave Function n = xx')  
plt.xlabel('x')
plt.ylabel('Wave Function u(x)')
plt.show()
