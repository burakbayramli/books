import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers

# Set up formatting for the movie files
Writer = writers['ffmpeg']
writer = Writer(fps=20, metadata=dict(artist='Llew'), bitrate=1800)

xmin,xmax = 0,100
ymin,ymax = -5,10
fig, ax = plt.subplots(figsize=(10,8))
xdata= np.linspace(xmin,xmax, 1000)
ln, = plt.plot([], [], 'r.',
                    markersize=.5,
                    animated=True,
                    label=r'x-nullcline: $y = \frac{a + x(ax - x^2 -1)}{4x}$')
ln2, = plt.plot([],[],
                    'b.',
                    markersize=.5,
                    animated=True, label=r'y-nullcline: $y =  1 + x^2$')
ax.set_facecolor(plt.cm.gray(.95))
ax.set_title('Animation of Belousovs model for $a = \{0 ... 15\}$ \n Strogatz 8.3.1, p.260')
ax.set_xlabel(r'$x$', size=14)
ax.set_ylabel(r'$y$', size=14)

def init():
    ax.set_xlim(xmin, 12)
    ax.set_ylim(ymin, ymax)
    return ln,ln2,

def update(frame):
    b = frame#3/5*frame - 25/frame
    ydata = (frame+xdata*(frame*xdata - xdata**2 -1))/(4*xdata)
    ln.set_data(xdata, ydata)
    y2data = (1 + xdata**2)
    ln2.set_data(xdata,y2data)
    return ln,  ln2,

ani = FuncAnimation(fig, update, frames=np.linspace(0,10, 1000),
                    init_func=init, interval=40, blit=True)
plt.grid(True)
plt.legend()
#ani.save('Hopf_8_3_2.mp4', writer=writer)
plt.show()