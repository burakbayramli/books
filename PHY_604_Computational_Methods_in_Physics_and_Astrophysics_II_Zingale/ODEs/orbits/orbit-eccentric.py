# compare Euler's method and the Euler-Cromer method for eccentric orbits
#
# M. Zingale

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.6)  # eccentricity = 0.6

# period
P = o.kepler_period()

hist_Euler = o.int_Euler(0.0125, P)
hist_EC = o.int_Euler_Cromer(0.0125, P)
#histRK2 = o.intRK2(0.0125, P)


plt.plot(hist_Euler.x, hist_Euler.y, label="Euler's method")
plt.plot(hist_EC.x, hist_EC.y, label="Euler-Cromer method")

# mark the Sun
plt.scatter([0], [0], s=250, marker=(20,1), color="k")
plt.scatter([0], [0], s=200, marker=(20,1), color="y")

# draw a vertical line that the semi-major axis should fall on
yy = np.linspace(-2.0, 2.0, 100)
plt.plot(0.0*yy, yy, ls=":", color="0.5")


leg = plt.legend(frameon=False, fontsize="small")

plt.xlim(-2, 2)
plt.ylim(-2, 2)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.tight_layout()
plt.savefig("orbit-eccentric.png", dpi=150)
