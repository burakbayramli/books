""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# HOcharge.py: Quantum particle in HO & E field with Visual

from visual import *

# Initialize psi, probability, potential
dx = 0.04;  dx2 = dx*dx;  k0 = 5.5*pi;  dt = dx2/20.;  xmax = 6.
xs = arange(-xmax,xmax+dx/2,dx)        # Array x positions
E = 210                                # Magnitude E field
g = display(width=500,height=250,
	title='Wave packet,harmonic well plus E field',range=10)
PlotObj= curve(x=xs, color=color.yellow, radius=0.1)
g.center = (0,2,0)                           

# Initial psi, V
psr = exp(-0.5*(xs/0.5)**2) * cos(k0*xs)    # Re part psi
psi = exp(-0.5*(xs/0.5)**2) * sin(k0*xs)    # Im part psi
V = 25.0*xs**2-E*xs                         # Electric potential

while True:   # Solution as time transpires
   rate(500)
   psr[1:-1] = psr[1:-1] - (dt/dx2)*(psi[2:]+psi[:-2]
   	   -2*psi[1:-1]) +dt*V[1:-1]*psi[1:-1]
   psi[1:-1] = psi[1:-1] + (dt/dx2)*(psr[2:]+psr[:-2]
   	   -2*psr[1:-1]) -dt*V[1:-1]*psr[1:-1]
   PlotObj.y = 4*(psr**2 + psi**2)
