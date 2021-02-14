""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# HOchargeMat.py charge in HO plus E field wi Matplotlib

from numpy import *
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

#                Initialization
dx = 0.06;  dx2 = dx*dx;  k0 = 5.5*pi;  dt = dx2/8.;  xmax = 6.
xs = np.arange(-xmax,xmax+dx/2,dx)             # x array
psr = exp(-0.5*(xs/0.5)**2) * cos(k0*xs)       # Re Psi
psi = exp(-0.5*(xs/0.5)**2) * sin(k0*xs)       # Im Psi
E = 70                                        # E field
v = 25.0*xs**2 -E*xs                         # V HO + E
fig=plt.figure()                 
ax = fig.add_subplot(111, autoscale_on=False,
	xlim=(-xmax,xmax), ylim=(0, 1.5))
ax.grid()                                    # Plot grid
plt.title("Charged Harmonic Oscillator in E Field")
line, = ax.plot(xs, psr*psr+psi*psi, lw=2)

def animate(dum):
   psr[1:-1] = psr[1:-1] - (dt/dx2)*(psi[2:]+psi[:-2]
   	   -2*psi[1:-1]) + dt*v[1:-1]*psi[1:-1]
   psi[1:-1] = psi[1:-1] + (dt/dx2)*(psr[2:]+psr[:-2]
   	   -2*psr[1:-1]) - dt*v[1:-1]*psr[1:-1]
   line.set_data(xs,psr**2+psi**2)
   return line,

ani = animation.FuncAnimation(fig, animate, 1, blit=True)
plt.show()