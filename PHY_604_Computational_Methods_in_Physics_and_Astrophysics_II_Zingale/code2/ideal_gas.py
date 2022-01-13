# MCMC model of a quantum ideal gas, following Newman Chapter 10

import sys
import numpy as np
import random
import matplotlib.pyplot as plt

class Atom(object):

    def __init__(self, nx, ny, nz):
        self.n = np.array([nx, ny, nz])

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
                reject = True
            else:
                dE = 0.5*np.pi**2*(-2*self.n[xyz] + 1)

        else:
            sys.exit("invalid move")


        # do we accept the move? (note that if we tried to go below n = 0,
        # then we set dE = 0
        if not reject:
            if random.random() < np.exp(-beta*dE):
                self.n[xyz] += s*1
            else:
                dE = 0.0
        else:
            dE = 0.0

        return dE

    def E(self):
        """ internal energy """
        return 0.5*np.pi**2*np.sum(self.n**2)


# initialize our gas -- all in the ground state
N_atoms = 2500
T = 10.0
beta = 1.0/T

atoms = []

for n in range(N_atoms):
    atoms.append(Atom(1, 1, 1))


# initial energy
E = np.sum(np.array([a.E() for a in atoms]))

print("initial energy = {}".format(E))


# do the MCMC interation -- for each iteration, 
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

# check our bookkeeping against a direct computation of the energy
Enew = np.sum(np.array([a.E() for a in atoms]))

plt.plot(E_history)
plt.savefig("E_history.png")

print("energy = {} {}".format(E, Enew))


