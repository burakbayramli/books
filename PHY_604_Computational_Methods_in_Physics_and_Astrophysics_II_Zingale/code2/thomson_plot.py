from __future__ import print_function

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
import sys

f = open(sys.argv[1], "r")

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# plot a sphere
phi = np.linspace(0, 2 * np.pi, 180)
theta = np.linspace(0, np.pi, 180)

R = 0.975
xm = R*np.outer(np.cos(phi), np.sin(theta))
ym = R*np.outer(np.sin(phi), np.sin(theta))
zm = R*np.outer(np.ones(np.size(phi)), np.cos(theta))
ax.plot_surface(xm, ym, zm, color="0.5", linewidth=0, alpha=0.5)

for line in f:
    if line.startswith("#"): continue
    coords = line.strip().split(":")[1]
    xyz = coords.split(",")
    x = float(xyz[0])
    y = float(xyz[1])
    z = float(xyz[2])
    ax.scatter(x, y, z)

plt.show()
