import numpy as np
import numpy.linalg
import matplotlib.pyplot as plt
import lorenz

# find the roots of the RHS of the lorenz Jacobian.


TOL = 1.e-8


def lorenz_roots(xvec0):
    """given an initial guess xvec0, find the roots using Newton's method"""

    xvec = xvec0.copy()

    err = 1.e100

    while err > TOL:
        print("iterating...")

        # get the jacobian
        J = lorenz.jac(xvec[0], xvec[1], xvec[2])

        # get the current RHS
        f = np.array(lorenz.rhs(xvec[0], xvec[1], xvec[2]))

        # solve for the correction: J dx = -f
        dx = np.linalg.solve(J, -f)

        xvec += dx
        err = np.max(np.abs(dx))

    print(" ")
    return xvec


def find_roots_and_plot():
    """ find a few roots and plot them in phase space """

    # find the roots -- the initial guesses here came from guessing
    # until we saw something different than before

    # initial guess for root
    xvec = np.array([1., 1., 1.])
    r1= lorenz_roots(xvec)

    xvec = np.array([10., 10., 10.])
    r2 = lorenz_roots(xvec)

    xvec = np.array([-10., -10., -10.])
    r3 = lorenz_roots(xvec)

    # plot the solution and the roots
    dt_init = 1.e-2
    tmax = 50.0
    err = 1.e-8

    # first run
    H = lorenz.integrate(xvec[0], xvec[1], xvec[2], dt_init, tmax, err)

    fig = plt.figure()
    ax = fig.gca(projection="3d")
    ax.plot(H.x, H.y, H.z, lw=1)

    ax.scatter(r1[0], r1[1], r1[2], marker="x", color="r")
    ax.scatter(r2[0], r2[1], r2[2], marker="x", color="r")
    ax.scatter(r3[0], r3[1], r3[2], marker="x", color="r")

    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")

    plt.savefig("lorenz-steadypoints.png", bbox_inches="tight", dpi=150)


if __name__ == "__main__":
    find_roots_and_plot()
