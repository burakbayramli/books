# gradient.py
# -------------------------------------------------------------------------
# Calculate and display the gradient of a two-dimensional Gaussian.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt

coords = np.linspace(-2, 2, 101)
X, Y = np.meshgrid(coords[::5], coords[::5])	# coarse grid for vector field
R = np.sqrt(X**2 + Y**2)
Z = np.exp(-R**2)
x, y = np.meshgrid(coords, coords)				# fine grid for contour plot
r = np.sqrt(x**2 + y**2)
z = np.exp(-r**2)

ds = coords[5] - coords[0]						# coarse grid spacing
dX, dY = np.gradient(Z, ds)						# calculate gradient

plt.figure()
plt.contourf(x, y, z, 25)					
plt.set_cmap('coolwarm')
plt.quiver(X, Y, dX.transpose(), dY.transpose(), scale=25, color='k')
plt.axis('tight')
plt.show()
