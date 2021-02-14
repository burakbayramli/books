""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# HOnumeric.py: 1-D HO wave functions via rk4

import numpy as np, matplotlib.pylab as plt
from rk4Algor import rk4Algor                                   
                                         
rVec = np.zeros((1000),float)              # x values for plot
psiVec = np.zeros((1000),float)         # Wave function values
fVec = [0]*(2); y = [0]*(2)               # Declare dimensions
n = 6                                            # n = npr L+1                

def f(x,y):                                          # ODE RHS                     
    fVec[0] = y[1]
    fVec[1] = -(2*n+1-x**2)*y[0]
    return fVec

if(n%2==0):   y[0]=1e-8                           # Set parity
else:  y[0]=-1e-8       
y[1] = 1.; i = 0                            
f(0.0,y)                                        # RHS at r = 0
dr = 0.01
for r in np.arange(-5,5,dr):         # Compute WF steps of dr
    rVec[i] = r
    y = rk4Algor(r, dr, 2, y, f) 
    psiVec[i] = y[0]    
    i = i+1                                  # Advance i & r
plt.figure()
plt.plot(rVec,psiVec) 
plt.grid()
plt.title('Harmonic Oscillator Wave Function n = 6')  
plt.xlabel('x')
plt.ylabel('$\psi(x)$')
plt.show()