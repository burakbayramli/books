""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# DielectMat.py; Matplotlib Animated E & B space to dielectric

from numpy import *
import numpy as np; import matplotlib.pyplot as plt
import matplotlib.animation as animation                      
Xmax = 401;  Ymax = 100;  Zmax = 100
eps = 4; dd = 0.5;  Xmax = 401         # Dielectric, stability param, Npts
Ex = zeros((Xmax),float);  Hy = zeros((Xmax),float)          # Declare E,H  
beta = zeros((Xmax),float)

for i in range (0,401):
    if i<201:   beta[i] = dd              # Free space, use stability cond
    else:       beta[i] = dd/eps                           # In dielectric        

z = arange(201)                        # Initial fields outside dielectric
xs = np.arange(1,Xmax-1)  
Ex[:201] = 0.5*sin(2*pi*z/100.)                       # Slice entire range
Hy[:201] =  0.5*sin(2*pi*z/100.)
fig = plt.figure()                             
ax = fig.add_subplot(111,autoscale_on=False,xlim=(1,Xmax-1),ylim=(-0.6,0.6))
ax.grid()
line, = ax.plot(xs,Ex[1:Xmax-1], lw=2)    

def animate(dum):              # Called by animation, NB "," in return line
   for x in range (1,Xmax-1):      
         Ex[x] = Ex[x] + beta[x]*(Hy[x-1]-Hy[x])   
         Hy[x] = Hy[x]+ dd*(Ex[x]-Ex[x+1])        
   line.set_data(xs,Ex[1:Xmax-1]) 
   return line, 
 
plt.title('Refraction and Reflection of Wave at Dielectric (on right)')  
plt.xlabel('z')
plt.ylabel('Ex')
p = plt.axvline(x=200,color='r')          # Vertical line separates regions
ani = animation.FuncAnimation(fig, animate,1,blit=True)   
plt.show()    