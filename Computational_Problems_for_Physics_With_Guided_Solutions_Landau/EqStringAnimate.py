""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
	 
# EqStringAnimate.py:    Animated leapfrog solution of wave equation

from visual import *

# Set up curve
g = display(width = 600, height = 300, title = 'Vibrating string')
vibst = curve(x = list(range(0, 100)), color = color.yellow)
ball1 = sphere(pos = (100, 0), color = color.red, radius = 2)
ball2 = sphere(pos = ( - 100, 0), color = color.red, radius = 2)
ball1.pos
ball2.pos
vibst.radius = 1.0

# Parameters
rho   = 0.01                                              
ten   = 40.                                               
c     = sqrt(ten/rho)                                  
c1    = c                                                 
ratio =  c*c/(c1*c1) # CFL criterium, set to 1 for stability

# Initialization
xi = zeros((101,3), float)                               
for i in range(0, 81):     xi[i, 0] = 0.00125*i;                      
for i in range (81, 101):  xi[i, 0] = 0.1 - 0.005*(i - 80)            
for i in range(0, 100):                                      # 1st t step
    vibst.x[i] = 2.0*i - 100.0                            
    vibst.y[i] = 300.*xi[i, 0]                           
vibst.pos                                                   # Draw string

# Later time steps
for i in range(1, 100): xi[i,1] = xi[i,0] + 0.5*ratio*(xi[i+1,0]+xi[i-1,0]-2*xi[i,0])
while 1:                               
    rate(50)                                             # Plotting delay
    for i in range(1, 100):              
        xi[i,2] = 2.*xi[i,1] - xi[i,0] + ratio * (xi[i+1,1]+xi[i-1,1]-2*xi[i, 1])
    for i in range(1, 100):
         vibst.x[i] = 2.*i - 100.0                       # Scale for plot
         vibst.y[i] = 300.*xi[i, 2]                             
    vibst.pos                                                
    for i in range(0, 101):
        xi[i, 0] = xi[i, 1]                                
        xi[i, 1] = xi[i, 2]                      

print("Done!")
