import numpy as np
import matplotlib.pyplot as plt
#From Strogatz p.129, Fig. 5.1.5
x,y = np.linspace(-10,10,100),np.linspace(-10,10,100)
X,Y = np.meshgrid(x,y)

a = -.5
U = a*X
V = -Y
speed = np.sqrt(U*U + V*V)

start = [[2,.75]]

fig0, ax0 = plt.subplots()

strm = ax0.streamplot(x,y, U, V, color=(.75,.90,.93), linewidth=.5)
strmS = ax0.streamplot(x,y, U, V, start_points=start, color="crimson", linewidth=1)
ax0.set_title('Linear System', size=14)
ax0.set_xlabel(r'$ \dot{x} $',size=14)
ax0.set_ylabel(r'$ \dot{v} $',size=14 )
ax0.text(-5,5,'a = {0}'.format(a))
plt.grid()
plt.show()