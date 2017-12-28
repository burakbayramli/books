import numpy as np
import matplotlib.pyplot as plt

x,y = np.linspace(-3,3,100),np.linspace(-3,3,100)
X,Y = np.meshgrid(x,y)
U = -1 - X**2 + Y
V = 1 + X - Y**2
speed = np.sqrt(U*U + V*V)

start = [[0,0], [1,2]]

fig0, ax0 = plt.subplots()

strm = ax0.streamplot(x,y, U, V, color=(.75,.90,.93))
strmS = ax0.streamplot(x,y, U, V, start_points=start, color="crimson", linewidth=2)

plt.show()