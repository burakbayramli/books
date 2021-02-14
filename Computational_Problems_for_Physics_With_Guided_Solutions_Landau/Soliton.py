""" From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""

# Soliton.py:      Korteweg de Vries equation for a soliton

from visual import *
import matplotlib.pylab as p;
from mpl_toolkits.mplot3d import Axes3D ;
import numpy

ds = 0.4;    dt = 0.1;    max = 2000
mu = 0.1;  eps = 0.2;    mx = 131                              
u   = zeros( (mx, 3), float); spl = zeros( (mx, 21), float); m = 1                                                     

for  i in range(0, 131):                  # Initial wave
    u[i, 0] = 0.5*(1 -((math.exp(2*(0.2*ds*i-5.))-1)/
    	    (math.exp(2*(0.2*ds*i-5.))+1)))
u[0,1] = 1.; u[0,2] = 1.; u[130,1] = 0.; u[130,2] = 0.  # Ends

for i in range (0, 131, 2): spl[i, 0] = u[i, 0]      
fac = mu*dt/(ds**3)              
print("Working. Please hold breath and wait while I count to 20")
for  i in range (1, mx-1):                      # First time step
    a1 = eps*dt*(u[i + 1, 0] + u[i, 0] + u[i - 1, 0])/(ds*6.)     
    if i > 1 and  i < 129: 
    	    a2 = u[i+2,0]+2.*u[i-1,0]-2.*u[i+1,0]-u[i-2,0]
    else:  a2 = u[i-1, 0] - u[i+1, 0]
    a3 = u[i+1, 0] - u[i-1, 0] 
    u[i, 1] = u[i, 0] - a1*a3 - fac*a2/3.        
for j in range (1, max+1):                     # Next time steps 
    for i in range(1, mx-2):
        a1 = eps*dt*(u[i+1,1] + u[i, 1] + u[i-1,1])/(3.*ds)
        if i > 1 and i < mx-2:
            a2 = u[i+2,1] + 2.*u[i-1,1]- 2.*u[i+1,1]-u[i-2,1]
        else:  a2 = u[i-1, 1] - u[i+1, 1]  
        a3        = u[i+1, 1] - u[i-1, 1] 
        u[i, 2] = u[i,0] - a1*a3 - 2.*fac*a2/3.
    if j%100 ==  0:             # Plot every 100 time steps
        for i in range (1, mx - 2): spl[i, m] = u[i, 2]
        print(m)  
        m = m + 1     
    for k in range(0, mx):     # Recycle array saves memory
        u[k, 0] = u[k, 1]                
        u[k, 1] = u[k, 2] 

x = list(range(0, mx, 2))           # Plot every other point
y = list(range(0, 21))      # Plot 21 lines every 100 t steps
X, Y = p.meshgrid(x, y)

def functz(spl):                           
    z = spl[X, Y]       
    return z

fig  = p.figure()                           # create figure
ax = Axes3D(fig)                                # plot axes
ax.plot_wireframe(X, Y, spl[X, Y], color = 'r')   # red wireframe
ax.set_xlabel('Positon')                           # label axes
ax.set_ylabel('Time')
ax.set_zlabel('Disturbance')
p.show()                   # Show figure, close Python shell
print("That's all folks!") 

