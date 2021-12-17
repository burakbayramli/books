import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.6)  # eccentricity = 0.6

# period
P = o.kepler_period()

histRK2 = o.int_RK2(0.0125, P)
histRK4 = o.int_RK4(0.0125, P)

# mark the Sun
plt.scatter([0], [0], s=250, marker=(20, 1), color="k")
plt.scatter([0], [0], s=200, marker=(20, 1), color="y")

plt.plot(histRK2.x, histRK2.y, label="2nd order RK")
plt.plot(histRK4.x, histRK4.y, label="4th order RK")


# draw a vertical line that the semi-major axis should fall on
yy = np.linspace(-2.0, 2.0, 100)
plt.plot(0.0*yy, yy, ls=":", color="0.5")


leg = plt.legend(frameon=False, fontsize="small")

plt.xlim(-1.5, 1.5)
plt.ylim(-2, 2)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.tight_layout()
plt.savefig("orbit-rk4.png", dpi=150)
