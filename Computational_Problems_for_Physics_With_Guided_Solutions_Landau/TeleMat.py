""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# TelehMat.py:  Lossless transmission line animation, Matplotlib

from numpy import *; from matplotlib import animation
import numpy as np, matplotlib.pyplot as plt

L = 0.1; C = 2.5; c = 1/sqrt(L*C); dt = 0.025;dx = 0.05
R = (c*dt/dx)**2          
V  = np.zeros( (101, 3), float)                # (Nx, Nt)
xx = 0
fig = plt.figure()                          # Figure to plot  
ax = fig.add_subplot(111,autoscale_on=False,xlim=(0,100), ylim=(-40,40))
ax.grid()                                                       
plt.title("Wave Transmission Via Telegrapher's Equations")
plt.xlabel("x");  plt.ylabel ("V(x,t)")
line, = ax.plot([],[], lw=2)     # x axis, y values, linewidth=2
                  
def init():                               
    line.set_data([],[])
    return line,
    
for i in arange (0,100):
    V[i,0] =  10*exp(-(xx**2)/0.1)   
    xx = xx + dx
    
for i in range(1, 100): 
	V[i,2] = V[i,0] + R*(V[i+1,1] + V[i-1,1] - 2*V[i,1])
V[:,0] = V[:,1];  V[:,1] = V[:,2]           # Recycle array
     
def animate(dum): 
        i =   arange(1, 100)   
        V[i, 2] = 2.*V[i,1]-V[i,0]+R*(V[i+1,1]+V[i-1,1]-2*V[i,1])
        line.set_data(i,V[i,2])
        V[:,0] = V[:,1]; V[:,1] = V[:,2]     # Recycle array
        return line,
# Plot 10000 frames, delay 10ms, blit=True redraw only changes   
ani = animation.FuncAnimation(fig, animate,init_func=init,frames=10000,
	interval=20,blit=True)  
plt.show()