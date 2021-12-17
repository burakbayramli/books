# integrate the stiff scalar equation, y' = -1.e3 ( y - exp(t)) - exp(t) ; y(0) = 0
# using adaptive, explicit RK4

from __future__ import print_function

import numpy as np
import matplotlib
import matplotlib.pyplot as plt

# adaptive timestepping parameters
S1 = 0.9
S2 = 4.0


class History(object):
    """ simple container to store the solution history """

    def __init__(self, t=None, y=None):
        self.t = np.array(t)
        self.y = np.array(y)


def RK4_singlestep(y0, t, dt, rhs):
    """ take a single RK-4 timestep from t to t+dt for the system
        ydot = rhs """

    # get the RHS at several points
    k1 = rhs(t, y0)
    k2 = rhs(t + 0.5*dt, y0 + 0.5*dt*k1)
    k3 = rhs(t + 0.5*dt, y0 + 0.5*dt*k2)
    k4 = rhs(t + dt, y0 + dt*k3)

    ynew = y0 + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)

    return ynew


def int_RK4(t0, y0, dt, err, tmax, rhs):
    """integrate the system using 4th order R-K method with an adaptive
    stepsize, to try to achieve the relative error err.  dt here is
    the initial timestep

    """

    # initial conditions
    t = 0.0
    y = y0

    # store the history for plotting
    tpoints = [t]
    ypoints = [y]

    # start with the old timestep
    dt_new = dt

    n_reset = 0

    while t < tmax:

        if err > 0.0:
            # adaptive stepping
            # iteration loop -- keep trying to take a step until
            # we achieve our desired error
            rel_error = 1.e10

            n_try = 0
            while rel_error > err:
                dt = dt_new
                if t+dt > tmax:
                    dt = tmax-t

                # take 2 half steps
                ytmp = RK4_singlestep(y, t, 0.5*dt, rhs)
                ynew = RK4_singlestep(ytmp, t+0.5*dt, 0.5*dt, rhs)

                # now take just a single step to cover dt
                ysingle = RK4_singlestep(y, t, dt, rhs)

                # ynew should be more accurate than ysingle since it
                # used smaller steps.

                # estimate the relative error now
                rel_error = abs((ynew-ysingle)/ynew)

                #print(t, dt, ynew, ysingle, rel_error)

                # adaptive timestep algorithm from Garcia (Eqs. 3.30
                # and 3.31)
                dt_est = dt*abs(err/rel_error)**0.2
                dt_new = min(max(S1*dt_est, dt/S2), S2*dt)

                n_try += 1

            if n_try > 1:
                # n_try = 1 if we took only a single try at the step
                n_reset += (n_try-1)


            # successful step
            t += dt

            # store
            tpoints.append(t)
            ypoints.append(ynew)

            # set for the next step
            y = ynew


    # return a OrbitHistory object with the trajectory
    H = History(tpoints, ypoints)
    H.n_reset = n_reset

    return H


def analytic(t):
    return np.exp(-t) - np.exp(-1.e3*t)


def rhs(t, y):
    return -1.e3*(y - np.exp(-t)) - np.exp(-t)


err = 1.e-7
dt_init = 0.05
tmax = 0.1

y0 = 0.0

h = int_RK4(0.0, y0, dt_init, err, tmax, rhs)

print("number of steps = ", len(h.t)-1)
print("number of resets = ", h.n_reset)

# plot the orbit
plt.plot(h.t, h.y, label="4th order adaptive RK")
plt.plot(h.t, analytic(h.t), label="analytic")

plt.legend(frameon=False, fontsize="small", loc="best")

plt.xlim(0.0, 0.1)

plt.xlabel("t")
plt.ylabel("y")

plt.savefig("stiff-rk4-adaptive.png", dpi=150)


# plot the timesteps
plt.clf()

dt = h.t[1:-1] - h.t[0:-2]
tmid = 0.5*(h.t[1:-1] + h.t[0:-2])
plt.scatter(tmid, dt)


ax = plt.gca()
ax.set_yscale('log')

plt.ylabel(r"$\tau$")
plt.xlabel(r"t")

plt.xlim(0.0, tmax)

plt.xlim(0.0, 0.1)

plt.xlabel("t")
plt.ylabel(r"$\tau$")

plt.tight_layout()
plt.savefig("stiff-dt-rk4-adaptive.png", dpi=150)
