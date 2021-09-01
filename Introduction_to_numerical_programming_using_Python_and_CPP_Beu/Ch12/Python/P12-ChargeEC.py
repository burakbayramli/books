# Charged particle orbiting about a fixed charge - Euler-Cromer method
from math import *
from ode import *
from graphlib import *

def Func(t, y, dy, f):                                # RHS of 2nd order ODEs
   factCoul = 14.3996517e0                          # e**2/(4*pi*eps0) [eV*A]
                                         # charge: e, energy: eV, force: eV/A
   r2 = y[1]*y[1] + y[2]*y[2]                # mass: u, distance: A, time: ps
   r = sqrt(r2)                                  # distance from force center
   fr = factCoul * q * Q / r2                                  # radial force
   f[1] = fr/m * y[1]/r
   f[2] = fr/m * y[2]/r

# main

m = 1e0                                                    # mass of particle
q = -1.e0; Q = 1.e0                                   # charges in units of e
x0 = 1e0; y0 = 0e0                                         # initial position
vx0 = 0e0; vy0 = 4.5e0                                     # initial velocity
tmax = 20e0                                                       # time span
ht = 0.01e0                                                  # time step size

n = 2                                              # number of 2nd order ODEs
nt = int(tmax/ht + 0.5) + 1                            # number of time steps
y = [0]*(n+1); dy = [0]*(n+1)                           # solution components
xt = [0]*(nt+1); yt = [0]*(nt+1)                            # plotting arrays

GraphInit(1200,600)

t = 0e0; it = 1                                                # Euler method
y[1] = x0; dy[1] = vx0                                       # initial values
y[2] = y0; dy[2] = vy0
xt[1] = y[1]; yt[1] = y[2]                               # store for plotting
while (t+ht <= tmax):                                      # propagation loop
   Euler2(t,ht,y,dy,n,Func)
   t += ht; it += 1
   xt[it] = y[1]; yt[it] = y[2]                          # store for plotting

Plot(xt,yt,it,"red",1,0.10,0.45,0.15,0.85,
     "t","y","Trajectory of charged particle - Euler")

t = 0e0; it = 1                                         # Euler-Cromer method
y[1] = x0; dy[1] = vx0                                       # initial values
y[2] = y0; dy[2] = vy0
xt[1] = y[1]; yt[1] = y[2]                               # store for plotting
while (t+ht <= tmax):                                      # propagation loop
   EulerCromer(t,ht,y,dy,n,Func)
   t += ht; it += 1
   xt[it] = y[1]; yt[it] = y[2]                          # store for plotting

Plot(xt,yt,it,"blue",1,0.60,0.95,0.15,0.85,
     "x","y","Trajectory of charged particle - Euler-Cromer")

MainLoop()
