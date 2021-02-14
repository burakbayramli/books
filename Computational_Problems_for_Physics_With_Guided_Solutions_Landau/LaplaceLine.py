""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# LaplaceLine.py:  Solve Laplace's eqtn within square 

import matplotlib.pylab as p, numpy
from mpl_toolkits.mplot3d import Axes3D;  from numpy import *;

Nmax = 100; Niter = 50
V = zeros((Nmax, Nmax), float)   
print ("Working hard, wait for the figure while I count to 60")

for k in range(0, Nmax-1):  V[0,k] = 100.0      # Line at 100V   
for iter in range(Niter):                                  
    if iter%10 == 0: print(iter)
    for i in range(1, Nmax-2):                                                
        for j in range(1,Nmax-2): 
            V[i,j] = 0.25*(V[i+1,j]+V[i-1,j]+V[i,j+1]+V[i,j-1])  
    print ("iter, V[Nmax/5,Nmax/5]", iter, V[Nmax/5,Nmax/5])
x = range(0, 50, 2);  y = range(0, 50, 2)                              
X, Y = p.meshgrid(x,y)                 

def functz(V):                                      # V(x, y) 
    z = V[X,Y]                        
    return z
    
Z = functz(V)                          
fig = p.figure()                            # Create figure
ax = Axes3D(fig)                                # Plot axes
ax.plot_wireframe(X, Y, Z, color = 'r')      # Red wireframe
ax.set_xlabel('X');  ax.set_ylabel('Y');  ax.set_zlabel('V(x,y)')
ax.set_title('Potential within Square V(x=0)=100V (Rotatable)')
p.show()                                          # Show fig