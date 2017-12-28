import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers

# Set up formatting for the movie files
Writer = writers['ffmpeg']
writer = Writer(fps=20, metadata=dict(artist='Llew'), bitrate=1800)

xmin,xmax = -3,3
ymin,ymax = -20,20
fig, ax = plt.subplots(figsize=(10,8))
ydata= ydata2 = np.linspace(ymin,ymax, 1000)
ln, = plt.plot([], [], 'r.',
                    markersize=1.5,
                    animated=True,
                    label=r'x-nullcline: $x = \frac{y}{(\mu+y^2)}$')
ln2, = plt.plot([],[],
                    'b:',
                    lw=.5,
                    animated=True, label=r'y-nullcline: $x =  -\mu y + y^3}$')
ax.set_facecolor(plt.cm.gray(.95))
ax.set_title('Animation of Hopf Bifurcation for $\mu = \{-1.5 ... 3\}$')
ax.set_xlabel(r'$x$', size=14)
ax.set_ylabel(r'$y$', size=14)

def init():
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    return ln, ln2,

def update(frame):
    xdata1 = ydata/(frame+ydata**2)
    xdata2 = -frame*ydata + ydata**3
    ln.set_data(ydata, xdata1)
    ln2.set_data(ydata,xdata2)
    return ln, ln2,

ani = FuncAnimation(fig, update, frames=np.linspace(-1.5,3, 500),
                    init_func=init, interval=60, blit=True)
plt.grid(True)
plt.legend()
#ani.save('Hopf_8_3_2.mp4', writer=writer)
plt.show()
