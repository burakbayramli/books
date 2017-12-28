import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers

# Set up formatting for the movie files
Writer = writers['ffmpeg']
writer = Writer(fps=20, metadata=dict(artist='Llew'), bitrate=1800)

xmin,xmax = -1,1
ymin,ymax = -1,1
fig, ax = plt.subplots(figsize=(10,8))
xdata= np.linspace(xmin,xmax, 1000)
ln, = plt.plot([], [], 'r.',
                    markersize=.5,
                    animated=True,
                    label=r'x-nullcline: $y = \mu x$')
ln2, = plt.plot([],[],
                    'b.',
                    markersize=.5,
                    animated=True, label=r'y-nullcline: $y =  \frac{x^2}{b(1 + x^2)}$')
ax.set_facecolor(plt.cm.gray(.95))
ax.set_title('Animation of Hopf Bifurcation for $\mu = \{-3 ... 3\}$ /n Strogatz 8.1.1, p.246')
ax.set_xlabel(r'$x$', size=14)
ax.set_ylabel(r'$y$', size=14)

def init():
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    return ln,ln2,

def update(frame):
    b = 2.5*frame
    ydata = (frame)*xdata
    ln.set_data(xdata, ydata)
    y2data = xdata/((1+xdata**2)*b)
    ln2.set_data(xdata,y2data)
    return ln,  ln2,

ani = FuncAnimation(fig, update, frames=np.linspace(-3,3, 500),
                    init_func=init, interval=60, blit=True)
plt.grid(True)
plt.legend()
#ani.save('Hopf_8_3_2.mp4', writer=writer)
plt.show()