import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.0)   # eccentricity = 0

# period
P = o.kepler_period()

histEuler1 = o.int_Euler(0.1, P)
histEuler2 = o.int_Euler(0.05, P)
histEuler3 = o.int_Euler(0.025, P)
histEuler4 = o.int_Euler(0.00125, P)


# mark the Sun
plt.scatter([0], [0], s=250, marker=(20, 1), color="k")
plt.scatter([0], [0], s=200, marker=(20, 1), color="y")

# draw the analytic solution
theta = np.linspace(0.0, 2.0*np.pi, 360)
plt.plot(np.cos(theta), np.sin(theta), ls=":", color="0.5")

plt.plot(histEuler1.x, histEuler1.y, label="dt = 0.1")

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.savefig("orbit-euler.png")

plt.plot(histEuler2.x, histEuler2.y, label="dt = 0.05")
plt.plot(histEuler3.x, histEuler3.y, label="dt = 0.025")
plt.plot(histEuler4.x, histEuler4.y, label="dt = 0.0125")


leg = plt.legend(frameon=False, fontsize="small")

plt.xlim(-2, 2)
plt.ylim(-2, 2)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.tight_layout()
plt.savefig("orbit-euler-dt.png", dpi=150)
