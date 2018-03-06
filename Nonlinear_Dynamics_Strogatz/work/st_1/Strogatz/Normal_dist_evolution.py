import numpy as np
import matplotlib.pyplot as plt

from matplotlib import animation, rc
from IPython.display import HTML
# First set up the figure, the axis, and the plot element we want to animate
x = np.linspace(0, .4, 100)
y0 = np.empty_like(x)
x0 = .5
fig, ax6 = plt.subplots(figsize=(10,8))
ax6.set_facecolor(plt.cm.gray(.9))
ax6.set_title(r'Evolution of iterated Normal distribution $x_{n + 1} = N(\mu \sigma \ x)$')
ax6.set_xlabel(r'$x_n$',size=14)
ax6.set_ylabel(r'$x_{n+1}$',size=14)
ax6.set_xlim(( 0, .4))
ax6.set_ylim((-.1, 15))
ax6.plot(x,x,'k',lw=.2)
ax6.grid()

def normal(mu,sigma,x):
    norm_ = 1/np.sqrt(2*np.pi*sigma*sigma)
    return norm_*np.exp(-(x-mu)**2/(2*sigma*sigma))

line, = ax6.plot([], [], 'b.', markersize=1)
# initialization function: plot the background of each frame
def init():
    line.set_data([], [])
    return (line,)
# animation function. This is called sequentially
def animate(i):
#    y0 = logistic(i,x)
    x0=.5
    for j in range(len(x)):
        y0[j] = normal(0,i,x0)
        x0=y0[j]
    line.set_data(x, y0)
    return (line,)
# call the animator. blit=True means only re-draw the parts that have changed.
anim = animation.FuncAnimation(fig, animate,
                               init_func=init,
                               frames=np.linspace(.5,0.01,600),
                               interval=10,
                               blit=True,
                               repeat_delay=500)
plt.show()
