# integrate the Lorenz model to demonstrate chaotic behavior
#
# This choice of parameters comes from Garcia, Ch. 3
#
# M. Zingale (2013-03-03)

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


# system parameters
sigma = 10.0
b = 8./3.
r = 28.0

# adaptive stepping
S1 = 0.9
S2 = 4.0


class IntHistory(object):
    """ a simple container to store the integrated history """

    def __init__(self, t=None, x=None, y=None, z=None):
        self.t = np.asarray(t)
        self.x = np.asarray(x)
        self.y = np.asarray(y)
        self.z = np.asarray(z)


def rhs(x, y, z):
    """ the RHS of the Lorenz system """

    xdot = sigma*(y - x)
    ydot = r*x - y - x*z
    zdot = x*y - b*z

    return xdot, ydot, zdot


def jac(x, y, z):
    """ the Jacobian of the Lorenz system """

    return np.array(
        [ [-sigma, sigma, 0.0],
          [r - z, -1.0, -x],
          [y, x, -b] ])


def rk4_singlestep(x0, y0, z0, dt):
    """ take a single step through dt using RK-4 integration """

    x = x0; y = y0; z = z0

    xdot1, ydot1, zdot1 = rhs(x, y, z)

    xdot2, ydot2, zdot2 = rhs(x + 0.5*dt*xdot1,
                              y + 0.5*dt*ydot1,
                              z + 0.5*dt*zdot1)

    xdot3, ydot3, zdot3 = rhs(x + 0.5*dt*xdot2,
                              y + 0.5*dt*ydot2,
                              z + 0.5*dt*zdot2)

    xdot4, ydot4, zdot4 = rhs(x + dt*xdot3,
                              y + dt*ydot3,
                              z + dt*zdot3)

    x += (dt/6.0)*(xdot1 + 2*xdot2 + 2*xdot3 + xdot4)
    y += (dt/6.0)*(ydot1 + 2*ydot2 + 2*ydot3 + ydot4)
    z += (dt/6.0)*(zdot1 + 2*zdot2 + 2*zdot3 + zdot4)

    return x, y, z


def integrate(x0, y0, z0, dt, tmax, err):
    """integrate the system to tmax using adaptive RK4, achieving an
    error err"""

    t = 0.0
    x = x0
    y = y0
    z = z0

    tpoints = [t]
    xpoints = [x]
    ypoints = [y]
    zpoints = [z]

    dt_new = dt

    while t < tmax:

        # adaptive stepping
        # iteration loop -- keep trying to take a step until
        # we achieve our desired error
        rel_error = 1.e10

        while rel_error > err:
            dt = dt_new
            if t + dt > tmax:
                dt = tmax - t

            # take 2 half steps
            xtmp, ytmp, ztmp = rk4_singlestep(x, y, z, 0.5*dt)
            xnew, ynew, znew = rk4_singlestep(xtmp, ytmp, ztmp, 0.5*dt)

            # now take just a single step to cover dt
            xsingle, ysingle, zsingle = rk4_singlestep(x, y, z, dt)

            # {x,y,z}new should be more accurate than {x,y,z}single,
            # since it used smaller steps.

            # estimate the relative error now
            rel_error = max(abs((xnew - xsingle)/xnew),
                            abs((ynew - ysingle)/ynew),
                            abs((znew - zsingle)/znew))


            # adaptive timestep algorithm from Garcia (Eqs. 3.30
            # and 3.31)
            dt_est = dt*abs(err/rel_error)**0.2
            dt_new = min(max(S1*dt_est, dt/S2), S2*dt)

        t += dt
        tpoints.append(t)
        xpoints.append(xnew)
        ypoints.append(ynew)
        zpoints.append(znew)

        # set for the next step
        x = xnew; y = ynew; z = znew

    H = IntHistory(tpoints, xpoints, ypoints, zpoints)
    return H


def run_and_plot():

    dt_init = 1.e-2
    tmax = 15.0
    err = 1.e-8

    x0 = 1.0
    y0 = 1.0
    z0 = 20.0

    # first run
    H1 = integrate(x0, y0, z0, dt_init, tmax, err)

    # second run with slightly different initial conditions
    z0 += 0.01

    H2 = integrate(x0, y0, z0, dt_init, tmax, err)

    plt.plot(H1.t, H1.x, label="x0 = y0 = 1, z0 = 20")
    plt.plot(H2.t, H2.x, label="x0 = y0 = 1, z0 = 20.01")

    plt.xlabel("t")
    plt.ylabel("x")

    plt.legend(fontsize="small", frameon=False)

    plt.tight_layout()
    plt.savefig("lorenz.png", dpi=150)

    plt.clf()


    # longer run for phase space
    x0 = 1.0
    y0 = 1.0
    z0 = 20.0

    H = integrate(x0, y0, z0, dt_init, tmax, err)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ax.plot(H.x, H.y, H.z)

    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")

    plt.tight_layout()
    plt.savefig("lorenz-phasespace.png", bbox_inches="tight", dpi=150)


if __name__ == "__main__":
    run_and_plot()
