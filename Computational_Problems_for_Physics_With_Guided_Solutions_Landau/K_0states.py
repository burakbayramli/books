""" From "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau &  MJ Paez, 
    Copyright R Landau, MJ Paez, 2017. 
    Please respect copyright & acknowledge our work."""

# K_0states.py: Wavepacket in double well

from visual.graph import *

dx = 0.04;   dx2 = dx*dx;   k0 = 0.3;  dt = dx2/18.;  Xmax = 8.0
nmax = int(2*Xmax/dx)  
X     = arange(-Xmax, Xmax, dx);        Rho = zeros((nmax+1),float)                   
V     = zeros((nmax+2),float);           p2 = zeros((nmax+1),float)
RePsi = zeros((nmax+1),float);        ImPsi = zeros((nmax+1),float) 

g = display(width=1200, height=1500, title='Wave Packet in Double Well',\
    background=color.white, foreground=color.black) 
PlotObj =   curve(x=X, color=color.red,   radius=0.02)        # Packet
Potential = curve(x=X, color=color.black, radius=0.02)     # Potential
RHL =  exp(-5.5*((X+4.5))**2)
RePsi = RHL * cos(k0*X) ;   ImPsi = RHL * sin(k0*X)      # Initial Psi                     
Rho  = RePsi*RePsi + ImPsi*ImPsi

i = 0                       
for x in arange (-Xmax+1,0,dx):                         # Left side V
    i = i+1
    V[i] = 20*(x+3.)**2     
for x in arange(0,Xmax+1,dx):                           # Right side V
    V[i] = 20*(x-3.)**2
    i = i+1

j = 0     
for x in arange(-Xmax, Xmax, dx):    
     PlotObj.x[j] = x                         
     PlotObj.y[j] = 5*Rho[j]-2                        # Scaled to fit
     Potential.x[j]= x
     Potential.y[j] = 0.03*V[j]                       # Scaled to fit
     j = j+1

for t in range(0,15000):                              # Time stepping        
      rate(900)
      for i in range(1, nmax-1): 
          RePsi[i] = RePsi[i]-dt*(ImPsi[i+1]+ImPsi[i-1]    
                     -2.*ImPsi[i])/(dx*dx)+dt*V[i]*ImPsi[i]
          p2[i] = RePsi[i]*RePsi[i] + ImPsi[i]*ImPsi[i]
      for i in range(1, nmax-1): 
           ImPsi[i] = ImPsi[i]+dt*(RePsi[i+1]+RePsi[i-1]\
                     -2.*RePsi[i])/(dx*dx)-dt*V[i]*RePsi[i]   
      PlotObj.y = 10*(RePsi**2 + ImPsi**2)                # Scaled rho
