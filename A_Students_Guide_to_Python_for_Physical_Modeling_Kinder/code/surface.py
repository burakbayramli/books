# surface.py
# -------------------------------------------------------------------------
# Create a three-dimensional surface plot.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# define grid of points
points = np.linspace(-1, 1, 101)
X, Y = np.meshgrid(points, points)
Z = X**2 + Y**2

# create and display surface plot
ax = Axes3D(plt.figure())
ax.plot_surface(X, Y, Z)

# Adjust rstride and cstride to use more or fewer points.
# The following command will use all points in X, Y, and Z:
# ax.plot_surface(X, Y, Z, rstride=1, cstride=1)

plt.show()
