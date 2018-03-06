import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers
from scipy import *
from scipy import integrate
from scipy.integrate import ode, odeint
import numpy as np

plt.close()
fig,ax = plt.subplots(1,1,figsize=(8,8))

#Vector field
xmin,xmax = -2,3
ymin,ymax = -2,2
gridpoints = 500
mu = -.92
x1,y1 = np.linspace(xmin,xmax,gridpoints),np.linspace(ymin,ymax,gridpoints)
X,Y = np.meshgrid(x1,y1 )
U = Y
V = mu*Y+X - X**2 +X*Y

# Starting values
start = [[0,1],[.264,0],[-.01,0]]

#Plotting. 
strm = ax.streamplot( X,Y,U, V,
                                    linewidth=.2)
strmS = ax.streamplot(x1,y1, U, V,
                                    start_points=start,
                                    color="crimson",
                                    linewidth=.5)
ax.set_facecolor(plt.cm.gray(.95))
ax.set_title('Homoclinic Bifurcation for $\mu = {}$'.format(mu))
ax.set_xlim([xmin,xmax])
ax.set_ylim([ymin,ymax])
ax.set_xlabel(r"$x$")
ax.set_ylabel(r"$y$")
ax.grid(True)
#ax.legend()
plt.show()
