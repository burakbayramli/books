# integrate an elliptical orbit with adaptive timestep 4th-order Runge-Kutta

from __future__ import print_function

import matplotlib.pyplot as plt
from orbit_adaptive import *

def do_orbits(adaptive=True):

    # circular orbit
    o = Orbit(1, 0.95)  # a, e

    # period
    P = o.kepler_period()

    if adaptive:
        err = 1.e-7
        suffix = "adaptive"
        dt_init = 0.05
    else:
        err = -1
        suffix = "nonadaptive"
        dt_init = 0.0005

    o.integrate(dt_init, err, P)

    print("number of steps = ", len(o.t)-1)
    print("number of resets = ", o.n_reset)

    # plot the orbit
    plt.clf()
    plt.plot(o.x, o.y, label="4th order RK")

    # mark the Sun
    plt.scatter([0], [0], s=250, marker=(20, 1), color="k")
    plt.scatter([0], [0], s=200, marker=(20, 1), color="y")

    plt.xlim(-1.5, 1.5)
    plt.ylim(-2.5, 0.5)

    ax = plt.gca()
    ax.set_aspect("equal", "datalim")

    plt.tight_layout()
    plt.savefig("orbit-rk4-{}.png".format(suffix), dpi=150)


    # plot the energy
    plt.clf()

    E = o.energy()
    plt.plot(o.t, E/E[0])

    plt.ylabel(r"$E(t)/E_0$")
    plt.xlabel(r"t")

    ax = plt.gca()
    ax.ticklabel_format(useOffset=False)

    plt.tight_layout()
    plt.savefig("energy-rk4-{}.png".format(suffix), dpi=150)


    # plot the timesteps
    plt.clf()

    dt = o.t[1:-1] - o.t[0:-2]
    tmid = 0.5*(o.t[1:-1] + o.t[0:-2])
    plt.scatter(tmid, dt)


    ax = plt.gca()
    ax.set_yscale('log')

    plt.ylabel(r"$\tau$")
    plt.xlabel(r"t")

    plt.ylim(1.e-5,1.e-1)

    plt.tight_layout()
    plt.savefig("dt-rk4-{}.png".format(suffix), dpi=150)


if __name__ == "__main__":
    do_orbits(adaptive=True)
    do_orbits(adaptive=False)
