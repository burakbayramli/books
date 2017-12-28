import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers
from scipy import *
from scipy import integrate
from scipy.integrate import ode, odeint
import numpy as np

plt.close()
fig,ax = plt.subplots(1,1,figsize=(8,8))

#Vector field
xmin,xmax = -2,2
ymin,ymax = -2,2
gridpoints = 1000
mu =.9

x1,y1 = np.linspace(xmin,xmax,gridpoints),np.linspace(ymin,ymax,gridpoints)
X,Y = np.meshgrid(x1,y1 )
#x = rcos(theta), x' = r'cos(theta) - r(sin(theta))theta'
#y = rsin(theta), y' = r'sin(theta) + r(cos(theta))theta'
#X = r, Y = theta
# # 
#For the system:
# U = r*(1 - r**2)
# V = mu - np.sin(theta)
#The converted coordinates are: 
X_prime = -mu*Y - X**3 - X*Y**2 + X + Y**2/(X*np.sqrt(1 + Y**2/X**2))
Y_prime = mu*X - X**2*Y - Y**3 + Y - Y/np.sqrt(1 + Y**2/X**2)
U = X_prime
V =  Y_prime
# Starting values
start = [[1,1],[-.2,0]]

#Plotting. 
strm = ax.streamplot( X,Y,U, V,
                                    linewidth=.2)
strmS = ax.streamplot(x1,y1, U, V,
                                    start_points=start,
                                    color="crimson",
                                    linewidth=.5)
ax.set_facecolor(plt.cm.gray(.95))
ax.set_title('Infinite Period Bifurcation for $\mu = {}$ \n Strogatz 8.4.2'.format(mu))
ax.set_xlim([xmin,xmax])
ax.set_ylim([ymin,ymax])
ax.set_xlabel(r"$x$")
ax.set_ylabel(r"$y$")
ax.grid(True)
#ax.legend()
plt.show()
