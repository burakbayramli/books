# integrate an elliptical orbit with fixed timestep 4th-order Runge-Kutta

from __future__ import print_function

import matplotlib.pyplot as plt
from orbit_adaptive import *

do_anim = False

# circular orbit
o = Orbit(1, 0.95)    # a, e

# period
P = o.kepler_period()

err = -1  # no adaptive stepping -- use the input dt always

dt = 0.0005
hist_RK4 = o.int_RK4(dt, err, P)
plt.plot(hist_RK4.x, hist_RK4.y, label=r"uniform $\tau = 0.0005$", color="b")

dt = 0.001
hist_RK4 = o.int_RK4(dt, err, P)
plt.plot(hist_RK4.x, hist_RK4.y, label=r"uniform $\tau = 0.001$", color="r")

dt = 0.002
hist_RK4 = o.int_RK4(dt, err, P)
plt.plot(hist_RK4.x, hist_RK4.y, label=r"uniform $\tau = 0.002$", color="g")


# mark the Sun
plt.scatter([0], [0], s=250, marker=(20,1), color="k")
plt.scatter([0], [0], s=200, marker=(20,1), color="y")

plt.xlim(-1.5,1.5)
plt.ylim(-2.5,0.5)

ax = plt.gca()
ax.set_aspect("equal", "datalim")

plt.legend(frameon=False, fontsize="small")

plt.savefig("orbit-rk4-nonadaptive-dts.png")

#-----------------------------------------------------------------------------

# plot frames for an animation
if do_anim:
    plt.clf()

    # mark the Sun
    plt.scatter([0],[0],s=250,marker=(20,1),color="k")
    plt.scatter([0],[0],s=200,marker=(20,1),color="y")

    plt.xlim(-1.5,1.5)
    plt.ylim(-2.5,0.5)

    ax = plt.gca()
    ax.set_aspect("equal", "datalim")

    dt = 0.0005
    histRK4 = o.intRK4(dt, err, P)

    for i in range(len(histRK4.t)):

        plt.plot(histRK4.x[0:i+1], histRK4.y[0:i+1])
    
        plt.savefig("orbit-anim-rk4-nonadaptive_%4.4d.png" % (i))

