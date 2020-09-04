# line3d.py
# -------------------------------------------------------------------------
# Create a three-dimensional parametric plot.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D		# import 3D plotting tool

fig = plt.figure()					# create a new figure
ax = Axes3D(fig)					# create 3D plotting object attached to figure
t = np.linspace(0, 5*np.pi, 501)	# define parameter for parametric plot

ax.plot(t * np.cos(t), t * np.sin(t), t)		# generate 3D plot
plt.show()
