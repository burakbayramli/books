# Oblique throw of an object with drag using the velocity Verlet method
from math import *
from ode import *

g = 9.81e0                                       # gravitational acceleration

def Forces(m, x, y, vx, vy):
   fx = -k * vx*abs(vx)                                    # force components
   fy = -k * vy*abs(vy) - m*g
   Epot = m*g*y                                            # potential energy
   return (fx, fy, Epot)

# main

m = 7e0                                                      # mass of object
k = 0.01e0                                             # velocity coefficient
x0 = 0e0; y0 = 3e0                                         # initial position
vx0 = 20e0; vy0 = 20e0                                     # initial velocity
tmax = 20e0                                                       # time span
ht = 0.001e0                                                 # time step size

out = open("throw.txt","w")                                # open output file
out.write("      t         x         y        vx        vy\n")

t = 0e0
x = x0; vx = vx0; ax = 0e0                                   # initial values
y = y0; vy = vy0; ay = 0e0
out.write(("{0:10.5f}{1:10.5f}{2:10.5f}{3:10.5f}{4:10.5f}\n"). \
          format(t,x,y,vx,vy))

while (t+ht <= tmax):                                      # propagation loop
   (x,y,vx,vy,ax,ay,Ekin,Epot) = Verlet2(ht,m,x,y,vx,vy,ax,ay,Forces)
   t += ht

   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}{3:10.5f}{4:10.5f}\n").
            format(t,x,y,vx,vy))
   if (y < 0.e0): break                      # stop if object hits the ground

out.close()
