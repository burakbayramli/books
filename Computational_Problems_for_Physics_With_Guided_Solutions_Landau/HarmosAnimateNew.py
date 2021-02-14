""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# HarmonsAnimate: Solve t-dependent Sch Eqt for HO wi animation

from visual import *

# Initialize wave function, probability, potential
dx = 0.04;    dx2 = dx*dx;  k0 = 5.5*pi;  dt = dx2/20.0;  
xmax = 6.0; beta = dt/dx2
xs = arange(-xmax,xmax+dx/2,dx)                    # Array x values

g = display(width=500, height=250, title='Wave Packet in HO Well')
PlotObj = curve(x=xs, color=color.yellow, radius=0.1)
g.center = (0,2,0)                                # Center of scene
                                              # Initial wave packet
R = exp(-0.5*(xs/0.5)**2) * cos(k0*xs)             # Array Re I
I = exp(-0.5*(xs/0.5)**2) * sin(k0*xs)             # Array Im I
V   = 15.0*xs**2                                     # The potential

while True:
   rate(500)
   R[1:-1] = R[1:-1] - beta*(I[2:]+I[:-2]-2*I[1:-1])+dt*V[1:-1]*I[1:-1]
   I[1:-1] = I[1:-1] + beta*(R[2:]+R[:-2]-2*R[1:-1])-dt*V[1:-1]*R[1:-1]
   PlotObj.y = 4*(R**2 + I**2)
