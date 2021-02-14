

from visual.graph import *

#initialize wave function, probability, potential
dx = 0.04;    dx2 = dx*dx;  k0 = 1.;  dt = dx2/18.0;  xmax = 6.0
xs = arange(-xmax,xmax,dx)             # array of x positions
nmax=300              # 300= 2*xmax/dx
g=display(width=500,height=500,title='Wave packet in two wells',background=color.white,\
  foreground=color.black)
PlotObj= curve(x=xs, color=color.red, radius=0.02) # to plot packet
potential=curve(x=xs, color=color.black, radius=0.02)  # to plot potential
v=zeros((nmax),float)     # potential
psr=zeros((nmax+1),float) # for real part wave function
psi=zeros((nmax+1),float) # for imaginary part wave function
i=0                       # counter
for x in arange (-xmax,0,dx):
    i=i+1
    v[i]=20*(x+2.5)**2    # left hand side potential
i=149                     # start right hand side potential
for x in arange(0,xmax,dx):   # right hand side potential
    v[i]=20*(x-2.5)**2
    i=i+1
prob=zeros((nmax+1),float)       # initial condition; wave packet
psr = exp(-5.5*((xs+4.5))**2) * cos(k0*xs)    # Re wave function Psi
psi = exp(-5.5*((xs+4.5))**2) * sin(k0*xs)    # Im wave function Psi
prob  = psr*psr + psi*psi                     # probability =wavefunction**2
j=0     
for x in arange(-xmax, xmax,dx):    
     PlotObj.x[j] = x             # x component            
     PlotObj.y[j] = 5*prob[j]-2   # 5* probability lowered 2
     potential.x[j]= x
     potential.y[j] = 0.03*v[j]-5  # scaled potential to plot
     j = j+1
for t in range(0,15000):           # packet deforms with time            
   #while True:
   rate(1000)
   psr[1:-1] = psr[1:-1] - (dt/dx2)*(psi[2:]+psi[:-2]-2*psi[1:-1]) +dt*v[1:-1]*psi[1:-1]
   psi[1:-1] = psi[1:-1] + (dt/dx2)*(psr[2:]+psr[:-2]-2*psr[1:-1]) -dt*v[1:-1]*psr[1:-1]
   PlotObj.y = 4*(psr**2 + psi**2)-1  # plot the wave packet with time
   
