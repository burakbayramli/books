#  Solves a Cauchy problem for a 2nd order ODE by Euler's method
#     y" + y = 0,   y(0) = y0, y'(0) = y0'
#  Equivalent problem: y[1] = y, y[2] = y'
#     y[1]' =  y[2],   y[1](0) = y0
#     y[2]' = -y[1],   y[2](0) = y0'
#----------------------------------------------------------------------------
from math import *
from ode import *

def Func(t, y, f):                                 # Right-hand sides of ODEs
   f[1] =  y[2]
   f[2] = -y[1]

# main

y0 = 0e0; dy0 = 1e0                         # initial values => y(t) = sin(t)
tmax = 100e0                                                      # time span
ht = 0.05e0                                                       # step size

n = 2                                              # number of 1st order ODEs
y = [0]*(n+1)                                           # solution components

out = open("ode.txt","w")                                  # open output file
out.write("      t         y1        y2      check\n")

t = 0e0
y[1] = y0; y[2] = dy0                                        # initial values
out.write(("{0:10.5f}{1:10.5f}{2:10.5f}{3:10.5f}\n"). \
          format(t,y[1],y[2],y[1]*y[1]+y[2]*y[2]))

while (t+ht <= tmax):                                      # propagation loop
   Euler(t,ht,y,n,Func)
   t += ht

   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}{3:10.5f}\n"). \
             format(t,y[1],y[2],y[1]*y[1]+y[2]*y[2]))
out.close()
