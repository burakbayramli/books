import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import cm

def step(x,x0):
    x0 = 0.6
    x1 = 0.8
    result = x - x0
    result[x-x1<x1] = 1.0            
    result[x<x0] = 0.0
    result[x>x1] = 0.0  
    return result

def gaussian(x,x0):
    s = 0.08
    s = s*s
    result = np.exp( -(x-x0)**2/s)
    return result

L = 1.0
n = 128 # cells
dx = L/n # n intervals
x = np.linspace(-3*dx/2, L + 3*dx/2, n+4) # include ghost cells - we will include 2 ghost cells on each side for high order schemes

# create arrays
phi = np.zeros(n+4) # cell centered quantity
f = np.zeros(n+4+1) # flux
u = np.ones(n+4+1) # velocity field - assumed to live on faces same as flux

x0 = 0.3
# u0 = np.zeros(N + 2)
# u0[1:-1] = np.sin(2*np.pi*x)
# u0 = np.zeros(N)
# phi0 = np.sin(np.pi*x)
phi0 = gaussian(x,x0) + step(x,x0)
# u0 = triangle(x,0.5,0.75,1)
# u0[0:N//2] = 1.0
plt.plot(x,phi0)

cfl =0.5
c = 1.0 # use a negative value for left traveling waves
dt = cfl*dx/abs(c)
print('dt=',dt)
print('dx=',dx)

# the k scheme
k = 0.5
# finite volume implementation with arrays for fluxes
t = 0
tend= L/abs(c)

sol = []
sol.append(phi0)
ims = []

fig = plt.figure(figsize=[5,3],dpi=200)
plt.rcParams["font.family"] = "serif"
plt.rcParams["font.size"] = 10
plt.rc('text')

# plt.grid()
plt.xlim([0.,L])
plt.ylim([-0.25,1.25])
plt.xlabel('$x$')
plt.ylabel('$\phi$')
plt.tight_layout()
# plot initial condition
plt.plot(x,phi0,'darkred')

i = 0
while t < tend:    
    phin = sol[-1]

#     if (i%16==0):
#         shift = int(np.ceil(c*(t-dt)/dx))
#         im = plt.plot(x[2:-2], np.roll(phin[2:-2], -shift) ,'k-o',markevery=2,markersize=3.5,markerfacecolor='deepskyblue',
#              markeredgewidth=0.25, markeredgecolor='k',linewidth=0.45, animated=True)
#         ims.append(im)
    
    # impose periodic conditions
    phin[-2] = phin[2]
    phin[-1] = phin[3]    
    phin[0] = phin[-4]        
    phin[1] = phin[-3]            
    
    phi = np.zeros_like(phi0)
    
    # predictor - take half a step and use upwind
    # du/dt = -c*du/dx
    if c >= 0:
        ϕc = phin[1:-2] # phi upwind
    else:
        ϕc = phin[2:-1] # phi upwind
    
    f[2:-2] = c*ϕc
    phi[2:-2] = phin[2:-2] - dt/2.0/dx*(f[3:-2] - f[2:-3])
    phi[-2] = phi[2]
    phi[-1] = phi[3]    
    phi[0] = phi[-4]        
    phi[1] = phi[-3]                
    
    # du/dt = -c*du/dx
    if c >= 0:
        ϕc = phi[1:-2] # phi upwind
        ϕu = phi[:-3]  # phi far upwind
        ϕd = phi[2:-1] # phi downwind
    else:
        ϕc = phi[2:-1] # phi upwind
        ϕu = phi[3:]  # phi far upwind
        ϕd = phi[1:-2] # phi downwind
        
    f[2:-2] = ϕc + (1-k)/4.0*(ϕc - ϕu) + (1+k)/4.0*(ϕd - ϕc)
    f = c*f # multiply the flux by the velocity
    # advect
    phi[2:-2] = phin[2:-2] - c * dt/dx*(f[3:-2] - f[2:-3]) #+ dt/dx/dx*diffusion
    t += dt    
    i+=1
    sol.append(phi)


# plt.annotate('k = '+ str(k), xy=(0.5, 0.8), xytext=(0.015, 0.9),fontsize=8)
# plt.legend(('exact','numerical'),loc='upper left',fontsize=7)
# ani = animation.ArtistAnimation(fig, ims, interval=100, blit=True,
#                                 repeat_delay=1000)

# ani.save('k-scheme-'+str(k)+'.mp4',dpi=300,fps=24)

plt.plot(sol[0], label='initial condition')
plt.plot(sol[-1], label='one residence time')
plt.legend()
plt.grid()

"""
Create Animation in Moving Reference Frame
"""
import matplotlib
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
matplotlib.use("Agg")
fig, ax = plt.subplots(figsize=(4,3),dpi=150) 
ax.grid(True)
f0 = sol[0]
line0, = ax.plot(x[2:-2], f0[2:-2] ,'r-',linewidth=0.75, animated=True)
line1, = ax.plot(x[2:-2], f0[2:-2] ,'k-o',markevery=2,markersize=3.5,markerfacecolor='deepskyblue',
             markeredgewidth=0.25, markeredgecolor='k',linewidth=0.45, animated=True)

ann = ax.annotate('time ='+str(round(t,3))+' s', xy=(2, 1), xytext=(40, 200),xycoords='figure points')
ax.annotate('k ='+str(k) + ' (k-scheme)', xy=(2, 1), xytext=(40, 190),xycoords='figure points')
plt.tight_layout()


def animate_moving(i):
    print('time=',i*dt)
    t = i*dt
    xt = x + i*1.1*c*dt
    line0.set_xdata(xt[2:-2])
    line1.set_xdata(xt[2:-2])    
    ax.axes.set_xlim(xt[0],0.0*dx + xt[-1])
    f = sol[i]
    ax.axes.set_ylim(1.1*min(f) - 0.1,1.1*max(f))
    ann.set_text('time ='+str(round(t,4))+'s (' + str(i)+ ').')
    shift =int(np.ceil(i*c*dt/dx))
    line1.set_ydata(np.roll(f[2:-2], -shift))

    f0 = sol[0]
    line0.set_ydata(f0[2:-2])
    return line0,line1


# Init only required for blitting to give a clean slate.
def init():
    line0.set_ydata(np.ma.array(x[2:-2], mask=True))
    line1.set_ydata(np.ma.array(x[2:-2], mask=True))    
    return line0,line1

ani = animation.FuncAnimation(fig, animate_moving, np.arange(0,len(sol),2*int(1/cfl)), init_func=init,
                              interval=20, blit=False)
print('done!')

ani.save('kscheme_' + str(k)+'.mp4',fps=24,dpi=300)

