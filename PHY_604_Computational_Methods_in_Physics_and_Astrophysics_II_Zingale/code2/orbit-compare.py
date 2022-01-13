# compare different ODE methods on the circular orbit problem
#
# M. Zingale (2013-02-19)

from __future__ import print_function

import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.0)   # eccentricity = 0

# orbital period
P = o.kepler_period()

hist_Euler = o.int_Euler(0.05, P)
hist_Euler_Cromer = o.int_Euler_Cromer(0.05, P)
hist_RK2 = o.int_RK2(0.05, P)
hist_RK4 = o.int_RK4(0.05, P)

plt.plot(hist_Euler.x, hist_Euler.y, label="Euler's method", color="k")

# mark the Sun
plt.scatter([0], [0], s=250, marker=(5,1), color="k")
plt.scatter([0], [0], s=200, marker=(5,1), color="y")


plt.plot(hist_Euler_Cromer.x, hist_Euler_Cromer.y, label="Euler-Cromer method",
           color="r")
plt.plot(hist_RK2.x, hist_RK2.y, label="R-K 2nd order",
           color="b")
plt.plot(hist_RK4.x, hist_RK4.y, label="R-K 4th order",
           color="g")

leg = plt.legend(frameon=False)
ltext = leg.get_texts()
plt.setp(ltext, fontsize='small')

plt.xlim(-2,2)
plt.ylim(-2,2)

ax = plt.gca()
ax.set_aspect("equal", "datalim")


plt.savefig("orbit-compare.png")
