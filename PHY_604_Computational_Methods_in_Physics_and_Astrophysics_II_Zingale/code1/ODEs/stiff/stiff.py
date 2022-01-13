# simple stiff ODE equation with analytic solution
#
# y' = -1.e3 (y - exp(-t) ) - exp(-t)
# y(0) = 0
#
# analytic solution is
#
# y(t) = exp(-t) - exp(-1e3 t)
#
# this example comes from Byrne & Hindemarsh (1986)
#
# M. Zingale (2013-02-22)

import numpy as np
import matplotlib.pyplot as plt

def analytic(t):
    return np.exp(-t) - np.exp(-1.e3*t)


def rhs(t, y):
    return -1.e3*(y - np.exp(-t)) - np.exp(-t)


def rk4(y0, dt, tmax):

    tsol = [0.0]
    ysol = [y0]

    t = 0.0
    y = y0

    while t < tmax:
        ydot1 = rhs(t, y)
        ydot2 = rhs(t+0.5*dt, y+0.5*dt*ydot1)
        ydot3 = rhs(t+0.5*dt, y+0.5*dt*ydot2)
        ydot4 = rhs(t+dt, y+dt*ydot3)

        y += (dt/6.0)*(ydot1 + 2.0*ydot2 + 2.0*ydot3 + ydot4)

        t += dt

        tsol.append(t)
        ysol.append(y)

    return np.array(tsol), np.array(ysol)


def backward_euler(y0, dt, tmax):

    tsol = [0.0]
    ysol = [y0]

    t = 0.0
    y = y0

    while t < tmax:
        # an implicit discretication: y^{n+1} - y^n = dt ydot^{n+1}
        # and then solve analytically for y^{n+1}:
        ynew = (y + 1.e3*dt*np.exp(-t) - dt*np.exp(-t))/ \
               (1.0 + 1.e3*dt)

        y = ynew
        t += dt

        tsol.append(t)
        ysol.append(y)

    return np.array(tsol), np.array(ysol)


y0 = 0.0

# plot analytic solution
tt = np.linspace(0.0, 1.0, 1000)


# plot RK4 solution -- we need a step of 1.e-3 or smaller to get this
# right, because that is the fastest timescale.  If you go above 2.e-3,
# it blows up severely
for dt in [1.e-3, 2.5e-3, 5.e-3]:
    tRK4, yRK4 = rk4(y0, dt, 1.0)

    plt.clf()
    plt.plot(tt, analytic(tt), label="analytic solution")
    plt.plot(tRK4, yRK4, label=r"R-K 4, $\tau = {}$".format(dt))

    plt.xlim(0.0, 0.1)

    plt.xlabel("t")
    plt.ylabel("y")

    leg = plt.legend(frameon=False, fontsize="medium")

    ax = plt.gca()
    ax.xaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))
    ax.yaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))

    if max(abs(yRK4.min()), abs(yRK4.max())) > 1.e4:
        plt.ylim(-1.e22, 0.0)

    plt.savefig("stiff-rk4-dt-{}.png".format(dt), dpi=150)


# implicit solution
plt.clf()

plt.plot(tt, analytic(tt), label="analytic solution")

tBE, yBE = backward_euler(y0, 5.e-3, 1.0)

plt.plot(tBE, yBE, label=r"backward-Euler, $\tau = 5\times 10^{-3}$")

tBE, yBE = backward_euler(y0, 1.e-2, 1.0)

plt.plot(tBE, yBE, label=r"backward-Euler, $\tau = 10^{-2}$")

plt.xlim(0.0, 0.1)

plt.xlabel("t")
plt.ylabel("y")

leg = plt.legend(loc=4)
ltext = leg.get_texts()
plt.setp(ltext, fontsize='small')
leg.draw_frame(0)

plt.tight_layout()
plt.savefig("stiff-be.png", dpi=150)
