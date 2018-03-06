import numpy as np
import matplotlib.pyplot as plt

x,y = np.linspace(0,5,100),np.linspace(0,2,100)
X,Y = np.meshgrid(x,y)
U = X
V = Y*(1-Y)
speed = np.sqrt(U*U + V*V)

start = [[.3,.15], [0.3,1], [.3,1.5],[3,1.5]]

fig0, ax0 = plt.subplots()

strm = ax0.streamplot(x,y, U, V, color=(.75,.90,.93))
strmS = ax0.streamplot(x,y, U, V, start_points=start, color="crimson", linewidth=2)

plt.show()