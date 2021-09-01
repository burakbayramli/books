# Angular motion of a nonlinear pendulum by the Euler-Cromer method
#    u" = -g/l * sin(u) - k * u',   u(0) = u0, u'(0) = u0'
from math import *
from ode import *
from graphlib import *

g = 9.81e0                                       # gravitational acceleration

def Func(t, u, v):
   return -g/l * sin(u) - k * v

# main

l = 1e0                                                     # pendulum length
k = 0e0                                                # velocity coefficient
u0 = 0.5e0*pi                                          # initial displacement
du0 = 0e0                                                # initial derivative
tmax = 20e0                                                       # time span
ht = 0.01e0                                                  # time step size

nt = int(tmax/ht + 0.5) + 1                            # number of time steps
ut = ut = [0]*(nt+1); vt = [0]*(nt+1)                   # arrays for plotting

GraphInit(1200,600)

t = 0e0; it = 1                                                # Euler method
u = u0; du = du0                                             # initial values
ut[1] = u; vt[1] = du                                    # store for plotting
while (t+ht <= tmax):                                      # propagation loop
   (u,du) = Euler1(t,ht,u,du,Func)
   t += ht; it += 1
   ut[it] = u; vt[it] = du                               # store for plotting

Plot(ut,vt,it,"red",1,0.10,0.45,0.15,0.85,"u (rad)","u' (rad/s)",
     "Trajectory of pendulum in phase space - Euler")

t = 0e0; it = 1                                         # Euler-Cromer method
u = u0; du = du0                                             # initial values
ut[1] = u; vt[1] = du                                    # store for plotting
while (t+ht <= tmax):                                      # propagation loop
   (u,du) = EulerCromer1(t,ht,u,du,Func)
   t += ht; it += 1
   ut[it] = u; vt[it] = du                               # store for plotting

Plot(ut,vt,it,"blue",1,0.60,0.95,0.15,0.85,"u (rad)","u' (rad/s)", \
     "Trajectory of pendulum in phase space - Euler-Cromer")

MainLoop()
