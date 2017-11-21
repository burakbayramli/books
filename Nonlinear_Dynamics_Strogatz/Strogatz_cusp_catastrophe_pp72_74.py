import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import numpy as np

plt.close()
#Create plots
fig, (ax0,ax1) = plt.subplots(2,1,sharex=True, figsize=(10,10))
lower,upper = -5,5
#Plot r v h as bifurcation.
x = np.linspace(-10,10,200) 
r = np.linspace(lower,upper,200,dtype=complex)
h = np.sqrt(r/3)*r*2/3
ax0.set_xlim(lower,upper)
ax0.plot(r,h,'b.',r,-h,'g.', alpha = .3)
ax0.set_title(r'$r$ = $\frac{2r}{3}\sqrt{\frac{r}{3}}$')
ax0.set_ylabel('h')
ax0.grid(True)
#Plot y as function of r and x
ax1.set_ylim(-4,4)
ax1.set_title(r'$y$ = $rx - x^3:   \   \{r \in{}: -5 ... 5\}$')
ax1.set_ylabel('y')
for i in range(lower,upper):
    y = i*x - x**3
    ax1.plot(-y,x, alpha = .4, label='r = {}'.format(i))
    plt.legend()
#3D
fig = plt.figure(figsize=(10,10))
ax2 = fig.gca(projection='3d')

# Make data.
x = np.arange(-1, 1, 0.01)
r = np.arange(-1, 1, 0.01)
X, R = np.meshgrid(x, r)
Z = X*R - X**3
# Plot the surface.
ax2.set_aspect('equal')
ax2.plot_surface(X, R, Z,
                        rstride=5,cstride=5, 
                        cmap=cm.inferno,
                        linewidth=1,
                        antialiased=True,
                        alpha=.5)
cset = ax2.contour(X, R, Z, zdir='z', 
                            offset=-2.0,
                            cmap=cm.coolwarm,
                            lw=2,
                            tight=True)
#cset = ax2.contour(X, R, Z, zdir='x', offset=0, cmap=cm.coolwarm)
cset = ax2.contour(X, R, Z, zdir='y',  offset=1, cmap=cm.coolwarm)

# Customize the z axis.
#ax2.set_zlim3d(-500, 500)
ax2.set_title('Cusp Catastrophe',size = 16)
ax2.set_xlabel('$x$', size=14)
ax2.set_ylabel('$r$',size=14)
ax2.set_zlabel('$y = r x - x^3 = -h $',size=14)
ax2.set_xticks([0]) 
ax2.set_yticks([0]) 
ax2.set_zticks([0])
ax2.grid(True)
ax2.xaxis.pane.set_edgecolor(None)
ax2.yaxis.pane.set_edgecolor(None)
ax2.zaxis.pane.set_edgecolor(None)
ax2.xaxis.pane.fill = False
ax2.yaxis.pane.fill = False
ax2.zaxis.pane.fill = True
#Rotate the plot
# for angle in range(0, 360):
#     ax2.view_init(30, angle)
#     plt.draw()
#     plt.pause(.001)
plt.show()