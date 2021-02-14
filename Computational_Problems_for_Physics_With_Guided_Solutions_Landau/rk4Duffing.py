""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# rk4Duffing.py solve ODE for Duffing Osc via rk4 & Matplotlib

import numpy as np, matplotlib.pylab as plt  
from math import *;  from rk4Algor import rk4Algor

tt = np.zeros((2000),float);  yy = np.zeros((2000),float);     
vy = np.zeros((2000),float);  y = [0]*(2); rhs = [0]*(2)     
a = 0.5;  b = -0.5;  g = 0.02;  F = 0.0008;   w = 1.         
h = 0.1                                     # Time step                                                   
i = 0; y[0] = 0.09;  y[1] =  0         # Initial x, velocity

def f(t,y):                                   # RHS function
    rhs[0] = y[1]
    rhs[1] = -2*g*y[1] - a*y[0] - b*y[0]**3 + F*cos(w*t)
    return rhs

for t in np.arange(0,200,h):                      # Time Loop    
    tt[i] = t
    y = rk4Algor(t,h,2,y,f)                         # Call rk4  
    yy[i] = y[0]                                         # x(t)
    vy[i] = y[1]                                         # v(t)
    i = i+1
fig, axes = plt.subplots(nrows=1, ncols=2,figsize=(12,5) )   
axes[0].plot(tt[1000:],yy[1000:])    # 1000 to avoid transients
axes[0].grid()                                            # x(t)
axes[0].set_title('Duffing Oscillator x(t)')         
axes[0].set_xlabel('t')
axes[0].set_ylabel('x(t)')
axes[1].plot(yy[1000:],vy[1000:])            
axes[1].grid()
axes[1].set_title('Phase Space Orbits for Duffing Oscillator')  
axes[1].set_xlabel('x(t)')
axes[1].set_ylabel('v(t)')      
plt.show()