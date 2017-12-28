import numpy as np
import matplotlib.pyplot as plt
from scipy import integrate
from scipy.integrate import ode, odeint

#From Strogatz p.40, Ex. 2.3.6
x,y = np.linspace(0,100,400),np.linspace(0,1,200)
X,Y = np.meshgrid(x,y)
s = 1/4
a = 1.31
U = X
V = s*(1-Y)*Y**a - (1-s)*Y*(1-Y)**a
speed = np.sqrt(U*U + V*V)

start = [[2,.75],[2,.4], [2,.6],[3,.9]]

fig0, ax0 = plt.subplots(figsize=(10,10))
#Integrate under curve
def odef(xx, t, s=s,a=a):
    """Right hand side for  ODEs."""
    #x1,x2 = xx
    return np.array([xx[0], s*(1-xx[1])*xx[1]**a - (1-s)*xx[1]*(1-xx[1])**a])

# Initial condition
xx0 = np.array([1, .5])

# Solve
t = np.linspace(0, 100, 400)
ys = odeint(odef, xx0, t, args=(s,a))

strm = ax0.streamplot(x,y, U, V, color=(.75,.90,.93), linewidth=.5)
strmS = ax0.streamplot(x,y, U, V, start_points=start, color="crimson", linewidth=1)
ax0.plot(ys[:,0],ys[:,1],label='scipy.ode')
ax0.set_title('Language Death model', size=14)
ax0.set_xlabel('Time in years')
ax0.set_ylabel('Proportion speaking a language' )
#Limits are essential for ensuring large values on't overwhelm the plot.
ax0.set_xlim(0,100)
ax0.set_ylim(0,1)
plt.legend()
plt.show()
