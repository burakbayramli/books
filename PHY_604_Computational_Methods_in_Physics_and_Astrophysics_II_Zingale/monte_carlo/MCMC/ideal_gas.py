# MCMC model of a quantum ideal gas, following Newman Chapter 10

import sys
import numpy as np
import random
import matplotlib.pyplot as plt
import scipy.optimize as optimize

class Atom(object):

    def __init__(self, nx, ny, nz, beta):
        self.n = np.array([nx, ny, nz])
        self.beta = beta

    def change_state(self, xyz, s):
        """ change the quantum number xyz (0 = x, 1 = y, 2 = z) by a 
            magnitude of 1 up (s = 1) or down (s = -1) and return the
            change in energy
        """

        reject = False

        # compute the updated energy
        if s == 1:
            dE = 0.5*np.pi**2*(2*self.n[xyz] + 1)

        elif s == -1:
            if self.n[xyz] == 1:
                # we are already in the ground state, so we cannot lower
                reject = True
            else:
                dE = 0.5*np.pi**2*(-2*self.n[xyz] + 1)

        else:
            sys.exit("invalid move")


        # do we accept the move? (note that if we tried to go below n = 0,
        # then we set dE = 0
        if not reject:
            if random.random() < np.exp(-self.beta*dE):
                self.n[xyz] += s*1
            else:
                dE = 0.0
        else:
            dE = 0.0

        return dE

    def E(self):
        """ internal energy """
        return 0.5*np.pi**2*np.sum(self.n**2)



def gas(N_atoms=1000, T=10.0):

    # initialize our gas -- all in the ground state
    beta = 1.0/T

    atoms = []

    for n in range(N_atoms):
        atoms.append(Atom(1, 1, 1, beta))

    # initial energy
    E = np.sum(np.array([a.E() for a in atoms]))

    # do the MCMC iteration -- for each iteration, 
    E_history = [E]

    N = 500000
    for iter in range(1, N):
        # choose the atom to move
        a = random.choice(atoms)

        # chose up or down
        s = random.choice([-1, 1])

        # and the direction
        d = random.randrange(3)

        E += a.change_state(d, s)
        E_history.append(E)

    return E_history

def main():

    E_10 = gas(T=10)
    E_20 = gas(T=20)

    plt.plot(E_10, label="T = 10")
    plt.plot(E_20, label="T = 20")

    plt.xlabel("iteration")
    plt.ylabel("E")
    plt.legend(frameon=False)

    plt.savefig("E_history.png", dpi=150)


if __name__ == "__main__":
    main()

