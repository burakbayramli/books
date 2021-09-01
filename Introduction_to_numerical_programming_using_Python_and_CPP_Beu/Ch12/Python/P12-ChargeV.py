# Charged particle orbiting about a fixed charge - Verlet method
from math import *
from ode import *
from graphlib import *

def Forces(m, x, y, vx, vy):
   factCoul = 14.3996517e0                          # e**2/(4*pi*eps0) [eV*A]
                                         # charge: e, energy: eV, force: eV/A
   r2 = x*x + y*y                            # mass: u, distance: A, time: ps
   r = sqrt(r2)                                  # distance from force center
   fr = factCoul * q * Q / r2                                  # radial force
   fx = fr * x/r                                           # force components
   fy = fr * y/r
   Epot = fr * r                                           # potential energy
   return (fx, fy, Epot)

# main

nn  = [0]*4; sty = [0]*4                       # end-indexes, styles of plots
col = [""]*4                                                # colors of plots

m = 1e0                                                    # mass of particle
q = -1.e0; Q = 1.e0                                           # in units of e
x0 = 1e0; y0 = 0e0                                         # initial position
vx0 = 0e0; vy0 = 4.5e0                                     # initial velocity
tmax = 20e0                                                       # time span
ht = 0.1e0                                                   # time step size

nt = int(tmax/ht + 0.5) + 1                            # number of time steps
tt = [0]*(3*nt+1); Et = [0]*(3*nt+1)                        # plotting arrays
xt = [0]*(nt+1); yt = [0]*(nt+1)

t = 0e0; it = 1
x = x0; vx = vx0                                             # initial values
y = y0; vy = vy0

(fx, fy, Epot) = Forces(m,x,y,vx,vy)    # initial forces and potential energy
ax = fx/m; ay = fy/m                                   # initial acceleration
Ekin = 0.5e0 * m * (vx*vx + vy*vy)                   # initial kinetic energy
xt[it] = x; yt[it] = y                                   # store for plotting
tt[it] = tt[nt+it] = t; tt[2*nt+it] = t
Et[it] = Ekin; Et[nt+it] = Epot; Et[2*nt+it] = Ekin + Epot

while (t < tmax):                                          # propagation loop
   (x,y,vx,vy,ax,ay,Ekin,Epot) = Verlet2(ht,m,x,y,vx,vy,ax,ay,Forces)
   t += ht; it += 1

   xt[it] = x; yt[it] = y                                # store for plotting
   tt[it] = tt[nt+it] = t; tt[2*nt+it] = t
   Et[it] = Ekin; Et[nt+it] = Epot; Et[2*nt+it] = Ekin + Epot

GraphInit(1200,600)

nn[1] =   nt; col[1] = "blue" ; sty[1] = 1                             # Ekin
nn[2] = 2*nt; col[2] = "green"; sty[2] = 1                             # Epot
nn[3] = 3*nt; col[3] = "red"  ; sty[3] = 1                             # Etot
MultiPlot(tt,Et,Et,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
          0.10,0.45,0.15,0.85,"t","Ek,Ep,Et","Energies of charged particle")
Plot(xt,yt,nt,"blue",1,0.60,0.95,0.15,0.85,
     "x","y","Trajectory of charged particle")

MainLoop()
