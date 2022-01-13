# integrate an elliptical orbit with fixed timestep 4th-order Runge-Kutta

from __future__ import print_function

import matplotlib.pyplot as plt
from orbit_adaptive import *

# circular orbit
o = Orbit(1, 0.95)    # a, e

# period
P = o.kepler_period()

err = -1  # no adaptive stepping -- use the input dt always
dt = 0.0005
histRK4 = o.int_RK4(dt, err, P)

print("number of steps = ", len(histRK4.t)-1)

# plot the orbit
plt.plot(histRK4.x, histRK4.y, label="4th order RK", color="b")
#plt.scatter(histRK4.x, histRK4.y, marker="x", color="b")

# mark the Sun
plt.scatter([0], [0], s=250, marker=(20,1), color="k")
plt.scatter([0], [0], s=200, marker=(20,1), color="y")

plt.xlim(-1.5, 1.5)
plt.ylim(-2.5, 0.5)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.savefig("orbit-rk4-nonadaptive.png")


# plot the energy
plt.clf()

E = histRK4.energy()
plt.plot(histRK4.t, E/E[0])

plt.ylabel(r"$E(t)/E_0$")
plt.xlabel(r"t")

plt.xlim(-0.05, 1.05)

ax = plt.gca()
ax.ticklabel_format(useOffset=False)

plt.savefig("energy-rk4-nonadaptive.png")


# plot the timesteps
plt.clf()

dt = histRK4.t[1:-1] - histRK4.t[0:-2]
tmid = 0.5*(histRK4.t[1:-1] + histRK4.t[0:-2])
plt.scatter(tmid, dt)


ax = plt.gca()
ax.set_yscale('log')

plt.ylim(1.e-5,1.e-1)

plt.savefig("dt-rk4-nonadaptive.png")
 

