# integrate an elliptical orbit with adaptive timestep 4th-order Runge-Kutta

from __future__ import print_function

import matplotlib.pyplot as plt
from orbit_adaptive import *

# circular orbit
o = Orbit(1, 0.95)  # a, e

# period
P = o.kepler_period()

err = 1.e-7
dt_init = 0.05
hist_RK4 = o.int_RK4(dt_init, err, P)

print("number of steps = ", len(hist_RK4.t)-1)
print("number of resets = ", hist_RK4.n_reset)

# plot the orbit
plt.plot(hist_RK4.x, hist_RK4.y, label="4th order RK", color="b")
#plt.scatter(histRK4.x, histRK4.y, marker="x", color="b")

# mark the Sun
plt.scatter([0], [0], s=250, marker=(20,1), color="k")
plt.scatter([0], [0], s=200, marker=(20,1), color="y")

plt.xlim(-1.5,1.5)
plt.ylim(-2.5,0.5)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.savefig("orbit-rk4-adaptive.png")


# plot the energy
plt.clf()

E = hist_RK4.energy()
plt.plot(hist_RK4.t, E/E[0])

plt.ylabel(r"$E(t)/E_0$")
plt.xlabel(r"t")

ax = plt.gca()
ax.ticklabel_format(useOffset=False)

plt.savefig("energy-rk4-adaptive.png")


# plot the timesteps
plt.clf()

dt = hist_RK4.t[1:-1] - hist_RK4.t[0:-2]
tmid = 0.5*(hist_RK4.t[1:-1] + hist_RK4.t[0:-2])
plt.scatter(tmid, dt)


ax = plt.gca()
ax.set_yscale('log')

plt.ylabel(r"$\tau$")
plt.xlabel(r"t")

plt.ylim(1.e-5,1.e-1)

plt.savefig("dt-rk4-adaptive.png")
 

