import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes

# old font defaults
mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'


x_smooth = np.linspace(0.0, np.pi, 500)
f_smooth = np.sin(x_smooth)


x = np.linspace(0.0, np.pi, 10)
f = np.sin(x)

i = 5
x_0 = x[i]
f_0 = f[i]

plt.plot(x_smooth, f_smooth)
plt.scatter(x, f)

plt.scatter(x_0, np.sin(x_0), color="r", zorder=10)

dx = x[1] - x[0]
x_d = np.linspace(x[i]-1.5*dx, x[i]+1.5*dx, 2)

d_exact = np.cos(x_0)

d_l = (np.sin(x[i]) - np.sin(x[i-1]))/dx
d_r = (np.sin(x[i+1]) - np.sin(x[i]))/dx
d_c = 0.5*(np.sin(x[i+1]) - np.sin(x[i-1]))/dx


# plot a line showing dx
dx = x[2] - x[1]
ann = plt.annotate('', xy=(x[1], f[1]-0.5*dx),  xycoords='data',
                   xytext=(x[2], f[1]-0.5*dx), textcoords='data',
                   arrowprops=dict(arrowstyle="<->",
                                   connectionstyle="bar,fraction=-0.4",
                                   ec="k",
                                   shrinkA=5, shrinkB=5,
                   ))

plt.text(0.5*(x[1]+x[2]), f[1]-0.8*dx, "$\Delta x$",
         horizontalalignment="center")

plt.plot([x[1], x[1]], [f[1]-0.5*dx, f[2]+0.5*dx], ls=":", color="0.5")
plt.plot([x[2], x[2]], [f[1]-0.5*dx, f[2]+0.5*dx], ls=":", color="0.5")

plt.plot(x_d, d_exact*(x_d - x_0) + f_0, color="0.5", lw=2, ls=":", label="exact")
plt.plot(x_d, d_l*(x_d - x_0) + f_0, label="left-sided")
plt.plot(x_d, d_r*(x_d - x_0) + f_0, label="right-sided")
plt.plot(x_d, d_c*(x_d - x_0) + f_0, label="centered")


ax = plt.gca()


# origin of the axes through 0
ax.spines['left'].set_position('zero')
ax.spines['right'].set_color('none')
ax.spines['bottom'].set_position('zero')
ax.spines['top'].set_color('none')
ax.spines['left'].set_smart_bounds(True)
ax.spines['bottom'].set_smart_bounds(True)
ax.xaxis.set_ticks_position('bottom')
ax.yaxis.set_ticks_position('left')

plt.legend(frameon=False, loc="best")

plt.ylim(-0.1, 1.1)

plt.tight_layout()


plt.savefig("derivs.pdf")



