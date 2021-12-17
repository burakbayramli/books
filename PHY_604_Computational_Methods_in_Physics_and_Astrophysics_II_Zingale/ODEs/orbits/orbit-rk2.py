import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.6)  # eccentricity = 0.6

# period
P = o.kepler_period()

histEC = o.int_Euler_Cromer(0.0125, P)
histRK2 = o.int_RK2(0.0125, P)

# mark the Sun
plt.scatter([0], [0], s=250, marker=(20, 1), color="k")
plt.scatter([0], [0], s=200, marker=(20, 1), color="y")

plt.plot(histEC.x, histEC.y, label="Euler-Cromer method", color="b")
plt.plot(histRK2.x, histRK2.y, label="2nd order RK", color="k")

# draw a vertical line that the semi-major axis should fall on
yy = np.linspace(-2.0, 2.0, 100)
plt.plot(0.0*yy, yy, ls=":", color="0.5")


leg = plt.legend(frameon=False, fontsize="small")

plt.xlim(-2, 2)
plt.ylim(-2, 2)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.tight_layout()
plt.savefig("orbit-rk2.png", dpi=150)
