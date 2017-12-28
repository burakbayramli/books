import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint
#Define plot axes.
fig,ax = plt.subplots(1,1,figsize=(10,8))
#Not declaring this way raises an error for streamplot but not quiver.
x,y = np.meshgrid( np.linspace(0,5,20),np.linspace(0,5,20) )

#Define function: Using lambda seems quite efficient.
# For dx/dt = x(1-x); p 35 Strogatz.
#Note that the x = x[0], y = dx/dt is in terms of x[1]
f = lambda x : [x[0], x[1]*(1- x[1])]
#Apply the function to the axes.
U,V = f([x,y])
#Both ways work/
# U = x
# V = y*(1-y)
#Plot the curve.
ax.streamplot(x,y,U,V, color='r',linewidth=.5)
ax.set_title('Phase and Trajectory plot')
ax.set_xlabel('x')
ax.set_ylabel('xdot')
#Make a new function also to integrate
def odef(vect,t):
    x =vect
    return x*(1-x)

#Calculate a trajectory with a starting point.
number_of_points = 200
t = np.linspace(0,5,number_of_points)
y0 = [.2,1,5]#[(0,2.5),(2,1),(3,1.2)]
N = len(y0)
tt = odeint(odef,y0,t)

#Plot a trajectory
ax.plot(t, tt[:,0],'g', lw=1)
ax.plot(t, tt[:,1],'b',lw=1)
ax.plot(t, tt[:,2],'k',lw=1)
#print(yint)
plt.show()
