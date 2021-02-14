""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

#  HarmosAnimate: Soltn of Sch Eqt for HO with animation 

from visual import *

dx = 0.04;    dx2 = dx*dx;  k0 = 5.5*pi;  dt = dx2/20.0;  xmax = 6.0
xs = arange(-xmax,xmax+dx/2,dx)              

g=display(width=500,height=250,title='Wave Packet in HO Potential')
PlotObj= curve(x=xs, color=color.yellow, radius=0.1)
g.center = (0,2,0)                                        # Scene center
psr = exp(-0.5*(xs/0.5)**2) * cos(k0*xs)                        # Re Psi
psi = exp(-0.5*(xs/0.5)**2) * sin(k0*xs)                        # Im Psi
v = 15.0*xs**2

while True:                                                # Runs forever
   rate(500)
   psr[1:-1] = psr[1:-1] - (dt/dx2)*(psi[2:]+psi[:-2]-2*psi[1:-1]) +dt*v[1:-1]*psi[1:-1]
   psi[1:-1] = psi[1:-1] + (dt/dx2)*(psr[2:]+psr[:-2]-2*psr[1:-1]) -dt*v[1:-1]*psr[1:-1]
   PlotObj.y = 4*(psr**2 + psi**2) 
