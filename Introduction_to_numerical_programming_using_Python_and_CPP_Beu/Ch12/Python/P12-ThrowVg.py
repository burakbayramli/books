# Oblique throw of an object with drag using the velocity Verlet method
from math import *
from ode import *
from graphlib import *

g = 9.81e0                                       # gravitational acceleration

def Forces(m, x, y, vx, vy):
   fx = -k * vx*abs(vx)                                    # force components
   fy = -k * vy*abs(vy) - m*g
   Epot = m*g*y                                            # potential energy
   return (fx, fy, Epot)

# main

m = 7.26e0                                                   # mass of hammer
R = 0.06e0                                                 # radius of hammer
x0 = 0e0; y0 = 3e0                                         # initial position
v0 = 29e0                                                  # initial velocity
phi = 45e0 * pi/180e0                                           # throw angle
vx0 = v0 *cos(phi); vy0 = v0*sin(phi)                      # initial velocity
rho = 1.2                                                    # density of air
Cd = 0.5e0                                                 # drag coefficient
k = 0.5e0*rho*(pi*R*R)*Cd                              # velocity coefficient
tmax = 20e0                                                       # time span
ht = 0.001e0                                                 # time step size

nt = int(tmax/ht + 0.5) + 1                            # number of time steps
tt = [0]*(nt+1); xt = [0]*(nt+1); yt = [0]*(nt+1)           # plotting arrays

t = 0e0; it = 1
x = x0; vx = vx0; ax = 0e0                                   # initial values
y = y0; vy = vy0; ay = 0e0
tt[1] = t; xt[1] = x; yt[1] = y                          # store for plotting

while (t+ht <= tmax):                                      # propagation loop
   (x,y,vx,vy,ax,ay,Ekin,Epot) = Verlet2(ht,m,x,y,vx,vy,ax,ay,Forces)
   t += ht; it += 1

   tt[it] = t; xt[it] = x; yt[it] = y                    # store for plotting
   if (y < 0.e0): break                      # stop if object hits the ground

print("xmax = {0:5.2f}".format(x))

GraphInit(1200,600)

Plot(tt,yt,it,"blue",1,0.10,0.45,0.15,0.85,"t","y","Altitude")
Plot(xt,yt,it,"blue",1,0.60,0.95,0.15,0.85,"x","y","Trajectory")

MainLoop()
