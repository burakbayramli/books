#  Solves a Cauchy problem for a 2nd order ODE by Euler's method
#     y" + y = 0,   y(0) = y0, y'(0) = y0'
from math import *
from ode import *
from graphlib import *

def Func(t, y, f):                                   # RHSs of 1st order ODEs
   f[1] =  y[2]                                         # y[1] = y, y[2] = y'
   f[2] = -y[1]

# main

y0 = 0e0; dy0 = 1e0                         # initial values => y(t) = sin(t)
tmax = 100e0                                                      # time span
ht = 0.05e0                                                       # step size

n = 2                                              # number of 1st order ODEs
nt = int(tmax/ht + 0.5) + 1                            # number of time steps
y = [0]*(n+1)                                           # solution components
y1 = [0]*(nt+1); y2 = [0]*(nt+1)                            # plotting arrays

GraphInit(1200,600)

t = 0e0; it = 1                                                # Euler method
y[1] = y0; y[2] = dy0                                        # initial values
y1[1] = y[1]; y2[1] = y[2]                               # store for plotting
while (t+ht <= tmax):                                      # propagation loop
   Euler(t,ht,y,n,Func)
   t += ht; it += 1
   y1[it] = y[1]; y2[it] = y[2]                          # store for plotting

Plot(y1,y2,nt,"red",2,0.10,0.45,0.15,0.85,"y","y'","Euler method")

t = 0e0; it = 1                            # Predictor-Corrector Euler method
y[1] = y0; y[2] = dy0                                        # initial values
y1[1] = y[1]; y2[1] = y[2]                               # store for plotting
while (t+ht <= tmax):                                      # propagation loop
   EulerPC(t,ht,y,n,Func)
   t += ht; it += 1
   y1[it] = y[1]; y2[it] = y[2]                          # store for plotting

Plot(y1,y2,nt,"blue",2,0.60,0.95,0.15,0.85,
     "y","y'","Euler Predictor-Corrector method")

MainLoop()
