import numpy as np
import matplotlib.pyplot as plt
from scipy import integrate
from scipy.integrate import ode, odeint

#From Strogatz p.201, Ex. 7.1.2
#Also, this recording goes through how to find  timescale estimates and parameter estimates
#using averaging. Very worthwhile. https://www.youtube.com/watch?v=ma6wOguLxnI&list=PLbN57C5Zdl6j_qJA-pARJnKsmROzPnO9V&index=11
fig0 = plt.figure(figsize=(10,10))
fig0.suptitle('van der Pol Oscillator', size=18)
ax0 = plt.subplot2grid((2,2), (0,0))
ax1 = plt.subplot2grid((2,2), (1,1))
ax2 = plt.subplot2grid((2,2), (1,0))#, colspan=2)
ax3 = plt.subplot2grid((2,2), (0,1))

# Initial condition
xx0 = np.array([-0.5, 0])
s=1.5

#Integrate under curve
def odef(xx, t, s=s):
    """Right hand side for  ODEs."""
    #x1,x2 = xx
    return np.array([xx[1], -s*(xx[0]**2-1)*xx[1]-xx[0]])

# Solve
t = np.linspace(0, 30, 500)
ys = odeint(odef, xx0, t, args=(s,))

#Plot x(t) v dx/dt
ax0.axis('equal')
ax0.set_title('Integral of $\ddot{x} = \mu \dot{x}(x^2 - 1) - x$', size=10)
ax0.plot(ys[:,0],ys[:,1],'b,',label='$x_0 = ({0},{1})$ with $\mu = {2}$'.format(xx0[0],xx0[1],s))
ax0.set_xlabel('$x$', size=14)
ax0.set_ylabel(r'$\dot{x}$',size=14 )
ax0.set_facecolor(plt.cm.gray(.95))
ax0.grid(True)
ax0.legend()
#Plot streamlines
x1,y1 = np.linspace(-3.5,3.5,200),np.linspace(-3.5,3.5,200)
X,Y = np.meshgrid(x1,y1 )
U = Y - s*(X**3/3-X)#See definition of x_dot and w_dot following Lienard transformation.
V = -X

start = [[-.5,0]]

ax3.set_title('Streamplot where $\mu$ = {0} with $x_0$ = ({1}, {2})'.format(s,start[0][0],start[0][1]), size=10)
ax3.set_ylabel(r'$y = \frac{w}{\mu}$',size=14 )
ax3.set_facecolor(plt.cm.gray(.95))
strm = ax3.streamplot( X,Y,U, V, linewidth=.3)
strmS = ax3.streamplot(x1,y1, U, V, start_points=start, color="crimson", linewidth=1)

#Plot nullcline pp.214-215.
#x_dot = 0 when w = uF(x)
#x2,y2 = np.linspace(-3.5,3.5,200),np.linspace(-3.5,3.5,200)
F_x = s*(x1**3/3 -x1) #p.215
U3 = Y - s*(X**3/3 - X)
V3 = -X
ax1.set_title(r'Nullclines for $y = \frac{x^3}{3} - x$', size=10)
ax1.plot(x1, F_x, 'r', lw=.5,label='Nullcline')
ax1.annotate(r'($-1, \frac{2}{3} \mu$)',
            xy=(-1,2/3*s),
            xytext=(-1.5, 1*s),
            arrowprops=dict(facecolor='black', width = .2,headwidth= 2, headlength=3,shrink=0.05))
ax1.annotate(r'($1, \frac{-2}{3} \mu$)',
            xy=(1,-2/3*s),
            xytext=(.5,-1*s),
            arrowprops=dict(facecolor='black', width = .2,headwidth= 2, headlength=3,shrink=0.05))
strm = ax1.quiver( X[::5,::5],Y[::5,::5],U3[::5,::5], V3[::5,::5],color='g',alpha=.5,scale_units='inches',scale=16)
ax1.set_xlabel('$x$', size=14)
ax1.set_ylabel(r'$y(x)$',size=14 )
ax1.set_facecolor(plt.cm.gray(.95))
ax1.set_xlim(-2.2,2.2)
ax1.set_ylim(-2.2,2.2)
ax1.legend()
ax1.grid(True)

#Plot x(t)
ax2.plot(t,ys[:,0], 'g,',lw=.3, label='$x$ (distance) over time')
ax2.plot(t,ys[:,1], 'b,',lw=.3, label='$\dot{x}$ (velocity) over time')
ax2.set_xlabel('$t$', size=14)
ax2.set_ylabel(r'$f(t)$',size=14 )
ax2.legend()
ax2.grid(True)
ax2.set_facecolor(plt.cm.gray(.95))

#plt.savefig('van_der_Pol.png')
#plt.tight_layout()
plt.show()
