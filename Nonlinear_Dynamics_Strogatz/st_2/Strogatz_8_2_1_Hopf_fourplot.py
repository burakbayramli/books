import matplotlib.pyplot as plt
from scipy import *
from scipy import integrate
from scipy.integrate import ode, odeint
import numpy as np

# Initial setup
xx0 = np.array([.25,1.5])
mu_ =[-.5,0,.07,1]
xmin,xmax = -2,2
ymin,ymax = -2,2
lw = 1.5
plt.close()
fig,([ax0,ax1],[ax2,ax3]) = plt.subplots(2,2,
                                                            figsize=(12,8),
                                                            sharex=True,
                                                            sharey=True)
ax0.set_facecolor(plt.cm.gray(.95))
ax1.set_facecolor(plt.cm.gray(.95))
ax2.set_facecolor(plt.cm.gray(.95))
ax3.set_facecolor(plt.cm.gray(.95))
#ax.axis('equal')

#Vector field
x1,y1 = np.linspace(xmin,xmax,200),np.linspace(ymin,ymax,200)
X,Y = np.meshgrid(x1,y1 )
#Plot1
#====================================
mu=mu_[0]
U = mu*X - Y +X*Y**2
V = X + mu*Y + Y**3
start = [[0.3,0],[1.4,-.35]]

#Plotting
strm = ax0.streamplot( X,Y,U, V,linewidth=.2)
try:
    strmS = ax0.streamplot(x1,y1, U, V, start_points=start, color="crimson", linewidth=.5)
except: # IndexError:
    print('Unable to plot initial value trajectories')
fig.suptitle('Hopf Bifurcation /n Strogatz 8.2.1, p.256', size=16)
ax0.set_title( 'a) Stable Spiral', size=12)
ax0.text(xmin+.25,ymax-.25, r'$\mu = {0}$'.format(mu),size=14)
ax0.plot(y1/(mu+y1**2),x1, 'k-.',lw=lw, label=r'x-nullcline: $x = \frac{y}{(\mu+y^2)}$')
ax0.plot(-mu*y1+y1**3,x1, 'k:', lw=lw, label=r'y-nullcline: $x =  -\mu y + y^3}$')
ax0.set_xlim([xmin,xmax])
ax0.set_ylim([ymin,ymax])
ax0.set_xlabel(r"$ x$",size=14)
ax0.set_ylabel(r"$ y$",size=14)
plt.grid(True)

#Plot 2
#===================================
mu= mu_[1]
U = mu*X - Y +X*Y**2
V = X + mu*Y + Y**3
start = [[0.3,0],[.6,.5]]

#Plotting
strm = ax1.streamplot( X,Y,U, V,linewidth=.2)
try:
    strmS = ax1.streamplot(x1,y1, U, V, start_points=start, color="crimson", linewidth=.5)
except: # IndexError:
    print('Unable to plot initial value trajectories')
ax1.set_title( 'b) Critical Point', size=12)
ax1.text(xmin+.25,ymax-.25, r'$\mu = {0}$'.format(mu),size=14)
ax1.plot(y1/(mu+y1**2),x1, 'k-.',lw=lw, label=r'x-nullcline: $x = \frac{y}{(\mu+y^2)}$')
ax1.plot(-mu*y1+y1**3,x1, 'k:', lw=lw, label=r'y-nullcline: $x =  -\mu y + y^3}$')
ax1.set_xlim([xmin,xmax])
ax1.set_ylim([ymin,ymax])
ax1.set_xlabel(r"$ x$",size=14)
ax1.set_ylabel(r"$ y$",size=14)
plt.grid(True)
#Plot 3
#===================================
mu= mu_[2]
U = mu*X - Y +X*Y**2
V = X + mu*Y + Y**3
start = [[0.3,0],[.6,.5]]

#Plotting
strm = ax2.streamplot( X,Y,U, V,linewidth=.2)
try:
    strmS = ax2.streamplot(x1,y1, U, V, start_points=start, color="crimson", linewidth=.5)
except: # IndexError:
    print('Unable to plot initial value trajectories')
ax2.set_title( 'c) Unstable Spiral', size=12)
ax2.text(xmin+.25,ymax-.25, r'$\mu = {0}$'.format(mu),size=14)
ax2.plot(y1/(mu+y1**2),x1, 'k-.',lw=lw, label=r'x-nullcline: $x = \frac{y}{(\mu+y^2)}$')
ax2.plot(-mu*y1+y1**3,x1, 'k:', lw=lw, label=r'y-nullcline: $x =  -\mu y + y^3}$')
ax2.set_xlim([xmin,xmax])
ax2.set_ylim([ymin,ymax])
ax2.set_xlabel(r"$ x$",size=14)
ax2.set_ylabel(r"$ y$",size=14)
plt.grid(True)
#Plot 4
#===================================
mu= mu_[3]
U = mu*X - Y +X*Y**2
V = X + mu*Y + Y**3
start = [[0.3,0],[.6,.5]]

#Plotting
strm = ax3.streamplot( X,Y,U, V,linewidth=.2)
try:
    strmS = ax3.streamplot(x1,y1, U, V, start_points=start, color="crimson", linewidth=.5)
except: # IndexError:
    print('Unable to plot initial value trajectories')
ax3.set_title( 'd) Unstable Spiral', size=12)
ax3.text(xmin+.25,ymax-.25, r'$\mu = {0}$'.format(mu),size=14)
ax3.plot(y1/(mu+y1**2),x1, 'k-.',lw=lw, label=r'x-nullcline: $x = \frac{y}{(\mu+y^2)}$')
ax3.plot(-mu*y1+y1**3,x1, 'k:', lw=lw, label=r'y-nullcline: $x =  -\mu y + y^3}$')
ax3.set_xlim([xmin,xmax])
ax3.set_ylim([ymin,ymax])
ax3.set_xlabel(r"$ x$",size=14)
ax3.set_ylabel(r"$ y$",size=14)
ax3.legend()
plt.grid(True)
plt.show()
