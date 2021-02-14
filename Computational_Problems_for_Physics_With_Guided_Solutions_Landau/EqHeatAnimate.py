""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# EqHeat.py Animated heat equation soltn via fine differences
 
from visual import *
from visual.graph import *

g = display(width = 600, height = 300, 
                      title = 'Cooling of Bar, T(t=0) = 100, T(x=0,L)=0')
# Temperature Curve, its parameters and labels
tempe = curve(x = list(range(0, 101)), color = color.red)
tempe.radius = 1.
yax = curve( pos = [(0,  - 20), (0, 40)], color = color.yellow )  
maxT = label( text = '100', pos = (0, 50), box = 0 )    
minT = label( text = '0',  pos = (0,  - 25), box = 0 )    
bar = curve( pos = [( - 100,  - 20), (100,  -20)], color=color.magenta)
thisbar = label( text = "Bar", pos = (15, -20), xoffset=15, yoffset=-15)
ball1 = sphere( pos = (100,  - 20), color = color.blue, radius = 4 ) 
ball2 = sphere( pos = ( - 100,  - 20), color = color.blue, radius = 4 )

# Parameters
Nx = 101                                               # Grid points in x
Dx = 0.01414                                                # x increment
Dt = 1.                                                     # t increment
KAPPA = 210.                                       # Thermal conductivity
SPH = 900.                                                # Specific heat
RHO = 2700.                                                     # Density
T = zeros( (Nx, 2), float)                         # Temp @ first 2 times

for ix in range (1, Nx - 1):                     # Initial temperature in the bar
     T[ix, 0] = 100.0
     
     
T[0, 0] = 0.0                                      # Ends of bar at T = 0
T[0, 1] = 0.                          
T[Nx - 1, 0] = 0.
T[Nx - 1, 1] = 0.0
cons = KAPPA/(SPH*RHO)*Dt/(Dx*Dx)          # Constant combo in algorthim

for i in range (0, Nx - 1):
     tempe.x[i] =  2.0*i  - 100.0                             # Scaled x's
     tempe.y[i] = 0.8*T[i, 0] - 20.0                          # Scaled y's (Temp)

while 1:
     rate(150)
     for ix in range (1, Nx - 1):
          T[ix, 1] = T[ix,0] +  cons*(T[ix+1,0] + T[ix-1,0] - 2.*T[ix,0])                                                        
     for i in range (0, Nx):
          tempe.x[i] = 2.0*i - 100.0            # Scale 0<x<100 -> -100<x<100
          tempe.y[i] = 0.6*T[i, 1] - 20.0
       
     for ix in range (1, Nx - 1):
          T[ix, 0] = T[ix, 1]               # Row of 100 positions at t = m
