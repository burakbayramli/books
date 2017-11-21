import matplotlib.pyplot as plt
from scipy import *
from scipy import integrate
from scipy.integrate import ode, odeint
import numpy as np

# Initial setup
xx0 = np.array([.25,1.5])
mu =0.1
xmin,xmax = -2,2
ymin,ymax = -2,2
plt.close()
fig,ax0 = plt.subplots(1,1,figsize=(8,8))
ax0.set_facecolor(plt.cm.gray(.95))
#ax.axis('equal')

#Vector field
x1,y1 = np.linspace(xmin,xmax,200),np.linspace(ymin,ymax,200)
X,Y = np.meshgrid(x1,y1 )
U = mu*X - Y +X*Y**2
V = X + mu*Y + Y**3
start = [[0.3,0],[.6,.5]]

#Plotting
strm = ax0.streamplot( X,Y,U, V,linewidth=.2)
try:
    strmS = ax0.streamplot(x1,y1, U, V, start_points=start, color="crimson", linewidth=.5)
except: # IndexError:
    print('Unable to plot initial value trajectories')
fig.suptitle('Hopf Bifurcation', size=16)
ax0.set_title( 'Strogatz 8.2.1, p.256', size=12)
ax0.text(xmin+.25,ymax-.25, r'$\mu = {0}$'.format(mu),size=14)
ax0.plot(y1/(mu+y1**2),x1, 'k-.',lw=.8, label=r'x-nullcline: $x = \frac{y}{(\mu+y^2)}$')
ax0.plot(-mu*y1+y1**3,x1, 'k:', lw=.8, label=r'y-nullcline: $x =  -\mu y + y^3}$')
ax0.set_xlim([xmin,xmax])
ax0.set_ylim([ymin,ymax])
ax0.set_xlabel(r"$ x$",size=14)
ax0.set_ylabel(r"$ y$",size=14)
plt.legend()
plt.grid(True)
plt.legend()
plt.grid(True)
plt.show()
