# shading.py
# -------------------------------------------------------------------------
# Demonstrate shading of surface plots using Matplotlib's LightSource.
# ------------------------------------------------------------------------- 
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Import Bessel function.
from scipy.special import jn

# Import colormaps.
from matplotlib import cm

# Import lighting object for shading surface plots.
from matplotlib.colors import LightSource

# Define grid of points.
points = np.linspace(-10, 10, 101)
X, Y = np.meshgrid(points, points)
R = np.sqrt(X**2 + Y**2)
Z = jn(0,R)

# Create an rgb array for single-color surfaces.
white = np.ones((Z.shape[0], Z.shape[1], 3))
red = white * np.array([1,0,0])
green = white * np.array([0,1,0])
blue = white * np.array([0,0,1])

# Set view parameters for all subplots.
azimuth = 45
altitude = 60

# Create empty figure.
fig = plt.figure(figsize=(18,12))

# -------------------------------------------------------------------------
# Generate first subplot.
# ------------------------------------------------------------------------- 
# Create a light source object for light from
# 0 degrees azimuth, 0 degrees elevation.
light = LightSource(0, 0)

# Generate face colors for a shaded surface using either
# a color map or the uniform rgb color specified above.

illuminated_surface = light.shade_rgb(red, Z)

# Create a subplot with 3d plotting capabilities.
# This command will fail if Axes3D was not imported.
ax = fig.add_subplot(2,2,1, projection='3d')
ax.view_init(altitude, azimuth)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, linewidth=0,
				antialiased=False, facecolors=illuminated_surface)

# -------------------------------------------------------------------------
# Repeat the commands above for the other three subplots, but use different
# illumination angles and colors.
# ------------------------------------------------------------------------- 
light = LightSource(90, 0)
illuminated_surface = light.shade_rgb(green, Z)

ax = fig.add_subplot(2,2,2, projection='3d')
ax.view_init(altitude, azimuth)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, linewidth=0,
				antialiased=False, facecolors=illuminated_surface)

# ------------------------------------------------------------------------- 
light = LightSource(90, 45)
illuminated_surface = light.shade_rgb(blue, Z)

ax = fig.add_subplot(2,2,3, projection='3d')
ax.view_init(altitude, azimuth)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, linewidth=0,
				antialiased=False, facecolors=illuminated_surface)

# ------------------------------------------------------------------------- 
light = LightSource(180, 45)
illuminated_surface = light.shade(Z, cmap=cm.coolwarm)

ax = fig.add_subplot(2,2,4, projection='3d')
ax.view_init(altitude, azimuth)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, linewidth=0,
				antialiased=False, facecolors=illuminated_surface)

# ------------------------------------------------------------------------- 
plt.tight_layout()
plt.savefig('shading.png')
