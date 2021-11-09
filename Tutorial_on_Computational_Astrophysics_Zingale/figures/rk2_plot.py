# make a plot of what the 2nd-order Runge-Kutta is doing using the
# example dy/dt = -y (this comes from Garcia).

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'

def rhs(y):
    """ return dy/dt """
    return -y

def exact(t):
    """ analytic solution """
    return np.exp(-t)


# frame 1 -- show the initial condition (y^n)

y0 = 1.0
t0 = 0.0

dt = 0.75

tt = np.linspace(t0, t0+2.0*dt, 100)


def start():
    """ default starting point """

    plt.plot(tt, exact(tt), label="analytic solution", color="k")

    # draw the timeline
    plt.plot(tt, 0*tt, color="k")

    # label the current point
    plt.scatter([t0], [y0], color="r")
    plt.text(t0, y0+0.03, r"$y^n$",
             horizontalalignment="left", color="r", fontsize=20)

    plt.plot([t0,t0], [0, y0], ls=":", color="0.5")
    plt.text(t0, -0.05, r"$t^n$",
             verticalalignment="top", horizontalalignment="center",
             fontsize=20)


    # illustrate where t^n+1 is
    plt.plot([t0+dt,t0+dt], [0, y0], ls=":", color="0.5")
    plt.text(t0+dt, -0.05, r"$t^{n+1}$",
             verticalalignment="top", horizontalalignment="center",
             fontsize=20)


start()

plt.axis("off")

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk2_initial.png", bbox_inches='tight', dpi=150)


# now draw the solution Euler would get
slope = rhs(y0)
tEuler = np.linspace(t0, t0+dt, 2)
yEuler = y0 + slope*(tEuler-t0)

plt.plot(tEuler, yEuler, label="Euler step")

plt.scatter([tEuler[1]], [yEuler[1]], color="r")
plt.text(tEuler[1]+0.015, yEuler[1], r"$y^{n+1}$",
         horizontalalignment="left", verticalalignment="center",
         color="r", fontsize=20)


leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk2_Euler.png", bbox_inches='tight', dpi=150)



# restart and show the 1/2 Euler step
plt.clf()
start()

plt.axis("off")

# now draw the solution Euler would get
slope = rhs(y0)
tEuler = np.linspace(t0, t0+0.5*dt, 2)
yEuler = y0 + slope*(tEuler-t0)

plt.plot(tEuler, yEuler, label="half-dt Euler step")

plt.scatter([tEuler[1]], [yEuler[1]], color="r")
plt.text(tEuler[1]+0.015, yEuler[1], r"$y^{\star}$",
         horizontalalignment="left", verticalalignment="center",
         color="r", fontsize=20)


# illustrate the slope at that position
slope2 = rhs(yEuler[1])
tSlope = np.linspace(tEuler[1]-0.2*dt, tEuler[1]+0.2*dt, 2)
ySlope = yEuler[1] + slope2*(tSlope - tEuler[1])

plt.plot(tSlope, ySlope, label=r"slope at $y^\star$", color="g", ls="--")

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk2_halfEuler.png", bbox_inches='tight', dpi=150)

# do the RK-2 step
ynew = y0 + dt*slope2

tFinal = np.linspace(t0, t0+dt, 2)
yRK2 = (ynew - y0)*(tFinal-t0)/dt + y0

plt.plot(tFinal, yRK2, label=r"R-K 2 solution", color="r")

plt.scatter(tFinal[1], yRK2[1], color="r")
plt.text(tFinal[1]+0.015, yRK2[1], r"$y^{n+1}$", color="r", fontsize=20)

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk2_final.png", bbox_inches='tight', dpi=150)
