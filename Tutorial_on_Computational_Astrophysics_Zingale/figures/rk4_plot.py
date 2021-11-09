# make a plot of what the 4th-order Runge-Kutta is doing using the
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
plt.savefig("rk4_initial.png", bbox_inches='tight', dpi=150)


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
plt.savefig("rk4_Euler.png", bbox_inches='tight', dpi=150)


#----------------------------------------------------------------------------
# show k1 slope
plt.clf()
start()

plt.axis("off")

k1 = rhs(y0)
tSlope = np.linspace(t0-0.2*dt, t0+0.2*dt, 2)
ySlope = y0 + k1*(tSlope - t0)

plt.plot(tSlope, ySlope, label=r"slope", color="g", ls="--", lw=2, zorder=10)
plt.text(t0-0.1*dt, y0, r"$k_1$", color="g", fontsize="large")

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk4_k1.png", bbox_inches='tight', dpi=150)


#----------------------------------------------------------------------------
# follow k1 to define k2

ytmp = y0 + k1*0.5*dt
k2 = rhs(ytmp)

# draw the k1 half step
plt.plot([t0, t0+0.5*dt], [y0, ytmp], color="b", label="half-dt k1 step")
plt.scatter(t0+0.5*dt, ytmp, color="b")

# draw slope there

tSlope = np.linspace(t0+0.5*dt-0.2*dt, t0+0.5*dt+0.2*dt, 2)
ySlope = ytmp + k2*(tSlope - (t0 + 0.5*dt))

plt.plot(tSlope, ySlope, color="g", ls="--", lw=2)
plt.text(t0+0.5*dt-0.1*dt, ytmp, r"$k_2$", color="g",
         verticalalignment="top", fontsize="large")

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk4_k2.png", bbox_inches='tight', dpi=150)


#----------------------------------------------------------------------------
# follow k2 to define k3
ytmp = y0 + k2*0.5*dt
k3 = rhs(ytmp)

# draw k2 half step
plt.plot([t0, t0+0.5*dt], [y0, ytmp], color="c", label="half-dt k2 step")
plt.scatter(t0+0.5*dt, ytmp, color="c")

# draw slope there
ySlope = ytmp + k3*(tSlope - (t0 + 0.5*dt))
plt.plot(tSlope, ySlope, color="g", ls="--", lw=2)
plt.text(t0+0.5*dt+0.05*dt, ytmp, r"$k_3$", color="g",
         verticalalignment="bottom", fontsize="large")

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk4_k3.png", bbox_inches='tight', dpi=150)


#----------------------------------------------------------------------------
# follow k3 to define k4
ytmp = y0 + k3*dt
k4 = rhs(ytmp)

# draw k3 full step
plt.plot([t0, t0+dt], [y0, ytmp], color="0.5", label="full-dt k3 step")
plt.scatter(t0+dt, ytmp, color="0.5")

# draw slope there
tSlope2 = np.linspace(t0+dt-0.2*dt, t0+dt+0.2*dt, 2)
ySlope = ytmp + k4*(tSlope2 - (t0 + dt))
plt.plot(tSlope2, ySlope, color="g", ls="--", lw=2)
plt.text(t0+dt-0.1*dt, ytmp, r"$k_4$", color="g",
         verticalalignment="top", fontsize="large")

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk4_k4.png", bbox_inches='tight', dpi=150)



#----------------------------------------------------------------------------
# final RK-4 step
ynew = y0 + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)


# draw full RK-4 step
plt.plot([t0, t0+dt], [y0, ynew], color="r", label="full 4th-order RK step")
plt.scatter(t0+dt, ynew, color="r")
plt.text(t0+1.05*dt, ynew+0.015, r"$y^{n+1}$", color = "r",
         horizontalalignment="left", fontsize=20)

leg = plt.legend(frameon=False, fontsize="medium", loc=1)

plt.xlim(t0-0.25*dt, t0+1.5*dt)
plt.ylim(-0.05, 1.2)

plt.tight_layout()
plt.savefig("rk4_final.png", bbox_inches='tight', dpi=150)
