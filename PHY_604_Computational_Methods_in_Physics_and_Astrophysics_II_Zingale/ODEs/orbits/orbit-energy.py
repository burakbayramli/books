# compare different ODE methods on the circular orbit problem
#
# M. Zingale (2013-02-19)

from __future__ import print_function

import matplotlib.pyplot as plt
from orbit import *

# circular orbit
o = Orbit(1.0, 0.3)   # eccentricity = 0

# orbital period
P = 10*o.kepler_period()

hist_Euler = o.int_Euler(0.05, P)
hist_EC = o.int_Euler_Cromer(0.05, P)
hist_RK2 = o.int_RK2(0.05, P)
hist_RK4 = o.int_RK4(0.05, P)
hist_VV = o.int_VVerlet(0.05, P)

#plt.plot(hist_Euler.t, hist_Euler.energy(), label="Euler's method")
plt.plot(hist_EC.t, hist_EC.energy(), label="Euler-Cromer method")
plt.plot(hist_RK2.t, hist_RK2.energy(), label="R-K 2nd order")
plt.plot(hist_RK4.t, hist_RK4.energy(), label="R-K 4th order")
plt.plot(hist_VV.t, hist_VV.energy(), label="velocity Verlet")

ax = plt.gca()
ax.grid(color="0.5", linestyle=":")

leg = plt.legend(frameon=False, fontsize="small")

ax.set_xlabel("t (periods)")
ax.set_ylabel("E(t)")

plt.tight_layout()
plt.savefig("orbit-energy.png", dpi=150)
