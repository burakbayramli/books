# compare circular orbits with Euler's method and the Euler-Cromer method

import numpy as np
import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.0)  # eccentricity = 0

# orbital period
P = o.kepler_period()

histEuler = o.int_Euler(0.0125, P)
histEC = o.int_Euler_Cromer(0.0125, P)


plt.plot(histEuler.x, histEuler.y, label="Euler's method", color="k")

# mark the Sun
plt.scatter([0], [0], s=250, marker=(20, 1), color="k")
plt.scatter([0], [0], s=200, marker=(20, 1), color="y")

# draw the analytic solution
theta = np.linspace(0.0, 2.0*np.pi, 360)
plt.plot(np.cos(theta), np.sin(theta), ls=":", color="0.5")

plt.plot(histEC.x, histEC.y, label="Euler-Cromer method")


leg = plt.legend()
ltext = leg.get_texts()
plt.setp(ltext, fontsize='small')
leg.draw_frame(0)

plt.xlim(-2,2)
plt.ylim(-2,2)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.tight_layout()
plt.savefig("orbit-ec.png", dpi=150)
