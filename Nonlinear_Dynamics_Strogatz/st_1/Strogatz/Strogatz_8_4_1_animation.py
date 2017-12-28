import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, writers
#from basic_units import radians
# # Set up formatting for the movie files
# Writer = writers['ffmpeg']
# writer = Writer(fps=20, metadata=dict(artist='Llew'), bitrate=1800)

#Polar stuff
fig = plt.figure(figsize=(10,8))
ax = plt.subplot(111,polar=True)
ax.set_title(r"Infinite Period Bifurcation: $\dot{r} \ and \ \dot{\theta} \ vary  \ for \ 0 \leq \mu < 1$", va='bottom')
ax.set_rticks([0.5, 1, 1.5, 2])  # fewer radial ticks
ax.set_facecolor(plt.cm.Dark2(.95))
ax.grid(True)
xT=plt.xticks()[0]
xL=['0',r'$\frac{\pi}{4}$',r'$\frac{\pi}{2}$',r'$\frac{3\pi}{4}$',\
    r'$\pi$',r'$\frac{5\pi}{4}$',r'$\frac{3\pi}{2}$',r'$\frac{7\pi}{4}$']
plt.xticks(xT, xL)
# Animation requirements.
ln, = plt.plot([], [], 'y.',
                    markersize=2.5,
                    alpha=1,
                    animated=True)
r = [0]
theta = [0]
mu = [.1]
def init():
    ax.set_xlim(0, 1)
    ax.set_ylim(0, .5)
    return ln,

def update(frame):
    r.append(frame*(1-frame**2))
    theta.append(mu[0] + np.sin(frame))
    if len(r)>=frame_num:
        global r
        r = [0] # OR r[:] = [] #This allows complete reset of global.
        global theta
        theta = [0] #theta[:]=[]
    ln.set_data(theta, r)
    #mu[0] = mu[0]+.1
    return ln,
frame_num = 200
ani = FuncAnimation(fig, update, frames=np.linspace(0,np.pi*2,frame_num),#fargs=(r,theta),
                    init_func=init, interval=10, blit=True,repeat=True)

plt.show()
