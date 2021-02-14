""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# Duffingrk4.py Solves ODE for Duffing Osc with rk4 & Matplotlib

import numpy as np
import matplotlib.pylab as plt          # Matplotlib for plotting
from math import *

n = 2   # number rhs eqnts                       
tt = np.zeros((10000),float)     # All times for plots 
yy = np.zeros((10000),float)     # All positions for plots
vy = np.zeros((10000),float)     # All velocities for plots
y = [0]*(2)     # Declare array for 2 values
h = 0.01        # Time step
a = 0.01
F = 0.1
w = 1.

# Force (RHS) function 
def f(t,y):                           
    rhs = [0]*(2)                      # coupled eqs.
    rhs[0] = y[1]
    rhs[1] = 0.5*y[0]*(1-y[0]**2)-a*y[1]+F*cos(w*t)
    return rhs
 
y[0] = 0.01
y[1] = 0.01
f(0.0,y)     # Call function with init conds.
i = 0

# Loop over all times, storing positions and velocities
for t in np.arange(0,100,h):    
    tt[i] = t
    yy[i] = y[0]
    vy[i] = y[1]
    y = rk4Algor(t, dt, 2, y, f)  
    i = i+1

plt.figure()
plt.plot(yy,vy) 
plt.grid()
plt.title('Duffing Phase Space')  
plt.xlabel('Position')
plt.ylabel('Velocity')
plt.show()