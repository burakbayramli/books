# solve the stiff linear system
#
# dY/dt = A Y
#
#           / -alpha   beta \
# with  A = |               |
#           \  alpha  -beta /
#
# this has the analytic solution for Y_B(0) = 0 of
#
# Y_B/Y_A = [ exp{(alpha+beta) t} - 1 ] / [ (beta/alpha) exp{(alpha+beta) t} + 1]
#
# this shows that a characteristic timescale is t ~ 1/(alpha + beta)
#
# and in the limit t -> infinity, Y_B/Y_A ~ alpha/beta
#
# This example came from Frank Timmes lecture notes
#
# With Backward-Euler, we have the linear system:
#
# (I - dt A) Y^{n+1} = Y^n
#
# with trapezoid, we have:
#
# (I - dt/2 A) Y^{n+1}  = (I + dt/2 A) Y^n
#
# also do Runge-Kutta 4th order and VODE
#
# M. Zingale

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import ode

alpha = 1.e9
beta = 1.e-5


def ode_mat():
    """ return the matrix A in the linear system of ODEs """
    return np.array([[-alpha,  beta],
                     [+alpha, -beta]], dtype=np.float64)


def rhs(t, Y):
    """ RHS of the system for explicit methods """
    return np.array([-alpha*Y[0] + beta*Y[1],
                     +alpha*Y[0] - beta*Y[1]])


def integrate(YA_0, YB_0, dt, tmax):
    """ perform a backward-Euler integration """

    YA = YA_0
    YB = YB_0

    tout = [0.0]
    yaout = [YA]
    ybout = [YB]

    t = 0.0
    while t < tmax:

        # create the matrix
        J = np.eye(2) - dt*ode_mat()

        b = np.array([YA, YB])

        # solve the linear system J x = b
        x = np.linalg.solve(J, b)

        YA = x[0]
        YB = x[1]

        t += dt

        tout.append(t)
        yaout.append(YA)
        ybout.append(YB)

    return np.array(tout), np.array(yaout), np.array(ybout)


def trap_integrate(YA_0, YB_0, dt, tmax):
    """ perform an implicit trapezoid integration:
        y^{n+1} = y^n + dt/2 [ f(y^n) + f(y^{n+1}) ] """

    YA = YA_0
    YB = YB_0

    tout = [0.0]
    yaout = [YA]
    ybout = [YB]

    t = 0.0
    while t < tmax:

        # create the matrix -- using dt/2
        J = np.eye(2) - 0.5*dt*ode_mat()

        # create the explicit source term
        b = np.dot(np.eye(2) + 0.5*dt*ode_mat(), np.array([YA, YB]))

        # solve the linear system J x = b
        x = np.linalg.solve(J, b)

        YA = x[0]
        YB = x[1]

        t += dt

        tout.append(t)
        yaout.append(YA)
        ybout.append(YB)

    return np.array(tout), np.array(yaout), np.array(ybout)


def rk4_integrate(YA_0, YB_0, dt, tmax):
    """ 4th-order explicit Runge-Kutta for comparison """

    YA = YA_0
    YB = YB_0

    tout = [0.0]
    yaout = [YA]
    ybout = [YB]

    t = 0.0
    while t < tmax:

        k1 = rhs(t, np.array([YA, YB]))
        k2 = rhs(t+0.5*dt, np.array([YA+0.5*dt*k1[0], YB+0.5*dt*k1[1]]))
        k3 = rhs(t+0.5*dt, np.array([YA+0.5*dt*k2[0], YB+0.5*dt*k2[1]]))
        k4 = rhs(t+dt, np.array([YA+dt*k3[0], YB+dt*k3[1]]))

        YA += (dt/6.0)*(k1[0] + 2.0*k2[0] + 2.0*k3[0] + k4[0])
        YB += (dt/6.0)*(k1[1] + 2.0*k2[1] + 2.0*k3[1] + k4[1])

        t += dt

        tout.append(t)
        yaout.append(YA)
        ybout.append(YB)

    return np.array(tout), np.array(yaout), np.array(ybout)


def vode_integrate(YA_0, YB_0, dt, tmax):
    """ integrate using the VODE method, don't take a step smaller than dt """

    r = ode(rhs, ode_mat).set_integrator("vode", method="bdf",
                                         with_jacobian=True,
                                         atol=1.e-16, rtol=1.e-13,
                                         nsteps=15000, order=5) #, min_step=dt)

    t = 0.0
    r.set_initial_value([YA_0, YB_0], t)

    tout = [t]
    yaout = [YA_0]
    ybout = [YB_0]

    while r.successful() and r.t < tmax:
        r.integrate(r.t+dt)

        tout.append(r.t)
        yaout.append(r.y[0])
        ybout.append(r.y[1])

    return np.array(tout), np.array(yaout), np.array(ybout)



def do_runs(dt_fac, with_vode=False):
    """ integrate our system using different methods """

    # Note: R-K works well with a timestep < tref.  For dt > tref, it
    # does horrible.  Backward-Euler always remains stable.

    tref = 1.0/(alpha + beta)  # characteristic timescale
    yequil = alpha/beta        # equilibrium YB/YA ratio

    dt = dt_fac*tref
    if dt_fac < 2.0:
        tmax = dt_fac*200*tref
    else:
        tmax = dt_fac*100*tref

    t, YA, YB = integrate(1.0, 0.0, dt, tmax)
    t2, YA2, YB2 = trap_integrate(1.0, 0.0, dt, tmax)
    trk, YArk, YBrk = rk4_integrate(1.0, 0.0, dt, tmax)

    if with_vode:
        tvode, YAvode, YBvode = vode_integrate(1.0, 0.0, dt, tmax)

    plt.clf()

    plt.scatter(t/tref, YB/YA/yequil,
                marker="x", s=25, label="backward-Euler")

    plt.scatter(t2/tref, YB2/YA2/yequil,
                marker="o", label="implicit trapezoid")

    plt.scatter(trk/tref, YBrk/YArk/yequil,
                marker="*", label="explicit R-K 4")

    if with_vode:
        plt.scatter(tvode/tref, YBvode/YAvode/yequil,
                    marker="d", label="VODE (implicit B-D)")

    plt.plot(t/tref, (np.exp((alpha+beta)*t) - 1.0)/
             ((beta/alpha)*np.exp((alpha+beta)*t) + 1.0)/yequil,
             ls="-", color="k", label="analytic")

    plt.legend(frameon=False, fontsize="x-small")

    plt.xlim(0.0, tmax/tref)

    plt.xlabel(r"$t \cdot (\alpha + \beta)$")
    plt.ylabel(r"$(Y_B/Y_A) / (\alpha/\beta)$")
    plt.title(r"$\Delta t = 0.5 / (\alpha + \beta)$")

    ax = plt.gca()
    ax.xaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))
    ax.yaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))

    plt.tight_layout()
    if with_vode:
        plt.savefig("stiff-linear-dt{}tref-VODE.png".format(dt_fac), dpi=150)
    else:
        plt.savefig("stiff-linear-dt{}tref.png".format(dt_fac), dpi=150)

if __name__ == "__main__":
    do_runs(0.5)
    do_runs(5.0)
    do_runs(5.0, with_vode=True)
