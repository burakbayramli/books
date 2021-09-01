#  Solves a Cauchy problem with adaptive step size control
from math import *
from ode import *
from graphlib import *

def Func(t, y, f):                                             # RHSs of ODEs
   f[1] = y[2]
   f[2] = (1e0 if t < 5e0 else -100e0) * y[1]

# main

y0 = 0e0; dy0 = 1e0                                          # initial values
tmax = 10e0                                                       # time span
ht = 0.01e0                                                       # step size
eps = 1e-8                                      # relative solution precision

n = 2                                              # number of 1st order ODEs
nt = int(tmax/0.001 + 0.5) + 1                         # number of time steps
y = [0]*(n+1)                                           # solution components
tp = [0]*(nt+1); yp = [0]*(nt+1); hp = [0]*(nt+1)     # for t-dependent plots

GraphInit(1200,600)

t = 0e0; it = 1                                                # Euler method
y[1] = y0; y[2] = dy0                                        # initial values
tp[1] = t; yp[1] = y[1]; hp[1] = ht                      # store for plotting

ht1 = ht                                            # initial step size guess
while (t+ht <= tmax):                                      # propagation loop
   ht = ht1                             # update initial step size with guess
   (ht, ht1) = RKFehlberg(t,ht,eps,y,n,Func)
   t += ht; it += 1
                                                         # store for plotting
   if (it <= nt): tp[it] = t; yp[it] = y[1]; hp[it] = ht

Plot(tp,yp,it,"blue",1,0.10,0.45,0.15,0.85,"t","y","Solution")
Plot(tp,hp,it,"blue",1,0.60,0.95,0.15,0.85,"t","h","Step size")

MainLoop()
