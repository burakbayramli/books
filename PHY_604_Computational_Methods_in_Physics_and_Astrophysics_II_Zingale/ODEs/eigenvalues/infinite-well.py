# Solve the Schrodinger equation for an infinite potential well.
#
# This is then just (in dimensionless coordinates):
#
# Psi'' = E Psi
#
# on xi = [0, 1]
#
# Here E is the energy eigenvalue.
#
# We have boundary conditions: Psi(0) = Psi(1) = 0
#
# We are free to pick Psi'(0) -- it only affects the normalization
# so we'll choose Psi'(0) = 1
#
# We solve this via shooting, integrating from the left and
# adjusting E to get the correct boundary at the right.
#
# The analytic solution for this has eigenvalues: E = (n pi)**2
#
# We'll define phi = psi'

from __future__ import print_function
import numpy as np
import matplotlib.pyplot as plt

def rhs(xi, y, E):
    """ our RHS function """

    psi = y[0]
    phi = y[1]

    return np.array([phi, -E*psi])

def integrate(y0, E, nsteps, a=0, b=1):
    """ integrate our system from xi = a to xi = b,
    with the choice of the eigenvalue = E using RK4
    with even steps, nsteps total """

    dxi = (b-a)/float(nsteps)

    y = y0

    xi_store = [a]
    psi_store = [y0[0]]

    for n in range(nsteps):
        xi = a + float(n)*dxi
        xihalf = a + float(n + 1./2.)*dxi
        xinew = a + float(n + 1.0)*dxi

        # RK4
        k1 = rhs(xi, y, E)
        k2 = rhs(xihalf, y + 0.5*dxi*k1, E)
        k3 = rhs(xihalf, y + 0.5*dxi*k2, E)
        k4 = rhs(xinew, y + dxi*k3, E)

        ynew = y + (dxi/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)

        y = ynew

        xi_store.append(xinew)
        psi_store.append(y[0])

    return y, xi_store, psi_store


def functional_plot():
    # let's just plot psi(E)(xi=1) -- this will show us all the values
    # and the zeros of this function will be the eigenvalues

    # initial conditions: y = [psi, phi]
    y0 = np.array([0.0, 1.0])
    nsteps = 100

    Ev = np.linspace(0.0, 1000, 1000)

    f = []
    for e in Ev:
        yy, _, _ = integrate(y0, e, nsteps)
        f.append(yy[0])

    plt.plot(Ev, f)

    ax = plt.gca()
    ax.spines['left'].set_position('zero')
    ax.spines['right'].set_color('none')
    ax.spines['bottom'].set_position('zero')
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_smart_bounds(True)
    ax.spines['bottom'].set_smart_bounds(True)
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')

    plt.xlabel(r"$\mathcal{E}$")
    plt.ylabel(r"$\psi^{(\mathcal{E})}(\xi=1)$")

    plt.savefig("infinite-well-f.png")


def solve(E_old, E_new):
    # do a secant iteration over our BVP varying the eigenvalue to
    # find the solution

    y0 = np.array([0.0, 1.0])
    nsteps = 100

    tol = 1.e-6
    dE = 1.e10

    y_old, _, _ = integrate(y0, E_old, nsteps)
    while np.abs(dE) > tol:
        y_new, xi_s, psi_s = integrate(y0, E_new, nsteps)

        dfdE = (y_new[0] - y_old[0])/(E_new - E_old)
        dE = -y_new[0]/dfdE

        E_old = E_new
        y_old = y_new

        E_new += dE


    print("eigenvalue is E = {}".format(E_new))

    plt.clf()
    plt.plot(xi_s, psi_s)
    plt.xlabel(r"$\xi$")
    plt.ylabel(r"$\psi$")
    plt.title(r"eigenvalue, $\mathcal{{E}}$ = {}".format(E_new))

    ax = plt.gca()
    ax.spines['left'].set_position('zero')
    ax.spines['right'].set_color('none')
    ax.spines['bottom'].set_position('zero')
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_smart_bounds(True)
    ax.spines['bottom'].set_smart_bounds(True)
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')

    ep = "{:6.2f}".format(E_new)
    plt.savefig("infinite-well-psi-E{}.png".format(ep.strip()))


if __name__ == "__main__":
    functional_plot()


    # this choice gets the first eigenvalue
    E_old = 1.0
    E_new = 2.0
    solve(E_old, E_new)

    # this choice gets the second eigenvalue
    E_old = 30
    E_new = 40
    solve(E_old, E_new)

    # this choice gets the third eigenvalue
    E_old = 100
    E_new = 110
    solve(E_old, E_new)

    E_old = 500
    E_new = 510
    solve(E_old, E_new)
