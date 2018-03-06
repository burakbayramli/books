import numpy as np
import matplotlib.pyplot as plt
#From Strogatz p.40, Ex. 2.3.6
x,y = np.linspace(-10,10,100),np.linspace(-10,10,100)
X,Y = np.meshgrid(x,y)

omega = -.5
U = Y
V = -omega**2*X
speed = np.sqrt(U*U + V*V)

start = [[2,.75]]

fig0, ax0 = plt.subplots(figsize=(10,8))

strm = ax0.streamplot(x,y, U, V, color=(.75,.90,.93), linewidth=.5)
strmS = ax0.streamplot(x,y, U, V, start_points=start, color="crimson", linewidth=1)
ax0.set_title('Spring-Mass', size=14)
ax0.set_xlabel(r'$ \dot{x} $',size=14)
ax0.set_ylabel(r'$ \dot{v} $',size=14 )
plt.grid()
plt.show()
