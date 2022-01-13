# Solve the traveling salesman problem using simulated annealing

from __future__ import print_function

import random
import matplotlib.pyplot as plt
import numpy as np

SMALL = 1.e-12

class Charge(object):
    """a single charge on the sphere"""

    def __init__(self, theta, phi):
        """theta is the angle from the z-axis, phi is the angle in the x-y
        plane

        """
        self.angles = [theta, phi]

        # compute x, y, z
        self.update_angles()

    def update_theta(self, theta):
        """if we update theta, we need to recompute the Cartesian coords"""
        self.update_angles(theta=theta)

    def update_phi(self, phi):
        """if we update theta, we need to recompute the Cartesian coords"""
        self.update_angles(phi=phi)

    def update_angles(self, theta=None, phi=None):
        """if we update theta and phi, we need to recompute the Cartesian coords"""
        if theta is not None:
            self.angles[0] = theta
        if phi is not None:
            self.angles[1] = phi

        self.x = np.sin(self.angles[0])*np.cos(self.angles[1])
        self.y = np.sin(self.angles[0])*np.sin(self.angles[1])
        self.z = np.cos(self.angles[0])

    def __str__(self):
        return "({:5.3f}, {:5.3f}) ".format(self.angles[0], self.angles[1])


class Thomson(object):
    """a collection of charges on a sphere"""

    def __init__(self, N):
        """ N is the number of charges to consider """

        self.N = N

        # we'll put them all equally spaced on the equator initially, cause why not...
        dphi = 2.0*np.pi/N

        self.charges = []
        for n in range(self.N):
            self.charges.append(Charge(np.pi/2, n*dphi))

        self.U = self.energy()

        # storage for move set
        self.angles_old = None

    def energy(self):
        """ return the potential energy of the charge configuration """
        U = 0.0
        for p in range(self.N):
            for q in range(p+1, self.N):
                U += 1.0/np.sqrt((self.charges[p].x - self.charges[q].x)**2 +
                                 (self.charges[p].y - self.charges[q].y)**2 +
                                 (self.charges[p].z - self.charges[q].z)**2 + SMALL)

        return U


    def move(self, T):
        """our move set will be to pick a charge at random, then pick theta
        or phi, and move it by a small amount

        """

        c = random.randrange(len(self.charges))
        j = random.randrange(2)

        # store the old info
        self.angles_old = self.charges[c].angles[:]

        U_old = self.U

        # use a Gaussian distribution with a small stddev
        da = np.random.normal(np.pi/16.0)
        if j == 0:
            self.charges[c].update_theta(min(np.pi,
                                             max(0.0, self.charges[c].angles[0] + da)))
        else:
            self.charges[c].update_phi(min(2.0*np.pi,
                                           max(0.0, self.charges[c].angles[1] + da)))

        U_new = self.energy()

        print(T, U_new)

        # check whether we should keep it
        if random.random() > np.exp(-(U_new - U_old)/T):
            # reject
            self.charges[c].update_angles(self.angles_old[0], self.angles_old[1])
        else:
            self.U = U_new


    def __str__(self):
        s = ""
        for c in self.charges:
            s += "{}".format(c)
        return s


def optimize(N, tau = 1.e4):
    """ find the Thomson config with minimium energy for N charges """

    # randomize the seed -- calling with no argument will use system time
    random.seed()

    thomson = Thomson(N)

    U_old = thomson.energy()

    print("initial energy = {}".format(U_old))

    Tmin = 1.e-3
    Tmax = 10.0

    # for storing the history
    Ts = []
    Us = []

    # cooling loop
    t = 0
    T = Tmax

    while T > Tmin:

        Ts.append(T)

        T = Tmax*np.exp(-t/tau)

        # perform a move
        thomson.move(T)

        Us.append(thomson.U)

        t += 1

    print("final energy = {}".format(Us[-1]))

    return Ts, Us


if __name__ == "__main__":

    T, U = optimize(20, tau=2.e4)
    plt.plot(U)
    plt.ylabel("U")
    plt.xlabel("iteration (-T)")
    ax = plt.gca()
    ax.set_xscale("log")
    plt.tight_layout()
    plt.savefig("thomson_sa.png", dpi=150)
