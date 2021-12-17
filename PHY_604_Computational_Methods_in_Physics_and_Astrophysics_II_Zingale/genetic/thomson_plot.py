from __future__ import print_function

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
import sys


class Charge(object):
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z


f = open(sys.argv[1], "r")

fig = plt.figure(1, (8.0, 8.0))
ax = fig.add_subplot(111, projection='3d')

# plot a sphere
phi = np.linspace(0, 2 * np.pi, 180)
theta = np.linspace(0, np.pi, 180)

R = 0.975
xm = R*np.outer(np.cos(phi), np.sin(theta))
ym = R*np.outer(np.sin(phi), np.sin(theta))
zm = R*np.outer(np.ones(np.size(phi)), np.cos(theta))
ax.plot_surface(xm, ym, zm, color="0.5", linewidth=0, alpha=0.6)

charges = []

for line in f:
    if line.startswith("#"): continue
    coords = line.strip().split(":")[1]
    xyz = coords.split(",")
    x = float(xyz[0])
    y = float(xyz[1])
    z = float(xyz[2])
    charges.append(Charge(x, y, z))
    ax.scatter(x, y, z, color="C1", s=40)

# draw lines connecting the charges
for j in range(len(charges)):
    for k in range(j, len(charges)):
        ax.plot([charges[j].x, charges[k].x], [charges[j].y, charges[k].y], [charges[j].z, charges[k].z], color="C0")

ax.set_aspect("equal")
plt.tight_layout()

plt.show()
