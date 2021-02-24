""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# SolitonAnimate.py: Solves KdeV equation for a soliton  

from visual import *

# Set up plot
g   = display(width = 600, height = 300, title = 'Soliton')
sol = curve(x = list(range(0, 131)), color = color.yellow)

# Parameters
sol.radius = 1.0                      # thickness of line
ds = 0.4                              # Delta x
dt = 0.1                              # Delta t
max = 2000                            # Numb t steps
mu = 0.1;                             # Mu from KdeV equation
eps = 0.2;                            # Epsilon from KdeV eq
mx = 131                              # grid in x max
fac = mu*dt/(ds**3)                   # combor

# Initialization
u = zeros( (mx, 3), float)           # Soliton amplitude

for  i in range(0, 131):                  # Initial wave
    u[i, 0] = 0.5*(1.-((math.exp(2*(0.2*ds*i-5.))-1)
    	    /(math.exp(2*(0.2*ds*i-5.))+1)))
u[0, 1]   = 1.
u[0, 2]   = 1.
u[130, 1] = 0.
u[130, 2] = 0.                             # End points

for  i in range(0, 131):
   sol.x[i] = 2*i - 130.0
   sol.y[i] = 50.0*u[i, 0] - 30

for  i in range (1, mx - 1 ):               # First time step
    a1 = eps*dt*(u[i + 1, 0] + u[i, 0] + u[i - 1, 0])/(ds*6.)     
    if i>1 and  i < 129:
    	    a2 = u[i+2,0] + 2.*u[i-1,0] - 2.*u[i+1,0] - u[i-2,0]
    else: a2 = u[i - 1, 0] - u[i + 1, 0]
    a3 = u[i + 1, 0] - u[i - 1, 0] 
    u[i, 1] = u[i, 0] - a1*a3 - fac*a2/3.

for  i in range(0, 131):
   sol.x[i] = 2*i - 130.0
   sol.y[i] = 50.0*u[i, 1] - 30

for j in range (1, max + 1):
    rate(150)                       # Following next time steps
    for i in range(1, mx - 2):
        a1 = eps*dt*(u[i + 1, 1] + u[i, 1] + u[i - 1, 1])/(3.*ds)
        if i>1 and i < mx-2: 
        	a2 = u[i+2,1] + 2*u[i-1,1]-2*u[i+1,1]-u[i-2,1]
        else: a2 = u[i-1, 1] - u[i+1, 1]  
        a3 = u[i + 1, 1] - u[i - 1, 1] 
        u[i, 2] = u[i, 0] - a1*a3 - 2.*fac*a2/3.
    if j%5 == 0:                    # Plot every 100 time steps
        for i in range (0, mx-2):
            sol.x[i] = 2*i - 130
            sol.y[i] = 50.*u[i, 2] - 30
        sol.pos 
    for k in range(0, mx):                      # Recycle array
        u[k, 0] = u[k, 1]             
        u[k, 1] = u[k, 2] 
                                                    # Finish plot
print("finished") 
