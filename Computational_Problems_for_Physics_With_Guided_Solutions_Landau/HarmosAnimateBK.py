""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# HarmonsAnimate: Solve t-dependent Sch Eqt for HO wi animation

from visual import *

# Initialize wave function, probability, potential
dx = 0.04;    dx2 = dx*dx;  k0 = 5.5*pi;  dt = dx2/20.0;  xmax = 6.0
xs = arange(-xmax,xmax+dx/2,dx)                    # Array x values

g = display(width=500, height=250, title='Wave Packet in HO Well')
PlotObj = curve(x=xs, color=color.yellow, radius=0.1)
g.center = (0,2,0)                                # Center of scene 
psr = exp(-0.5*(xs/0.5)**2) * cos(k0*xs)          # Initial RePsi Array
psi = exp(-0.5*(xs/0.5)**2) * sin(k0*xs)          # Initial ImPsi Array
V   = 15.0*xs**2                                     # The potential

while True:
   rate(500)
   psr[1:-1] = psr[1:-1]-(dt/dx2)*(psi[2:]+psi[:-2]-2*psi[1:-1])+dt*V[1:-1]*psi[1:-1]
   psi[1:-1] = psi[1:-1]+(dt/dx2)*(psr[2:]+psr[:-2]-2*psr[1:-1])-dt*V[1:-1]*psr[1:-1]
   PlotObj.y = 4*(psr**2 + psi**2)
