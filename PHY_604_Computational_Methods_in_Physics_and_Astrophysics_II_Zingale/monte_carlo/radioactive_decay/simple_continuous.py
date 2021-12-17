# model radioactive decay using the uniform random number distribution
#
# this is very crude -- it only considers time in units of 1/2 lives

import random
import numpy as np
import matplotlib.pyplot as plt

# number of atoms
N = 1000


class Atom(object):
    def __init__(self, t_half):
        self.decayed = 0
        self.t_half = t_half

    def decay(self, t):
        # the probability that we have decayed is 
        #   p(t) = 1 - 2**(-t/t_half) 
        # p(t) = 1 means that we definitely decayed,
        # p(t) = 2/3 means we would have decayed 2 times out of 3.  
        # So pick a random number, r, between [0, 1), and if r < p(t),
        # then we decayed
        p = 1.0 - 2.0**(-t/self.t_half)
        if not self.decayed:
            if random.random() < p:
                self.decayed = 1


# create our initial parent atoms
t_half = 1.0
atoms = []
for n in range(N):
    atoms.append(Atom(t_half))


# loop over halflifes
n_half_lives = 5

times = np.linspace(0.0, n_half_lives*t_half, 100)

dt = times[1] - times[0]

N_decayed = []

for t in times:
    for a in atoms:
        a.decay(dt)

    N_decayed.append(len([a for a in atoms if a.decayed == 1]))


plt.scatter(times, np.array(N_decayed), s=10, marker="x", label="decayed")
plt.scatter(times, N - np.array(N_decayed), s=10, marker="x", label="remaining")

plt.plot(times, N - N*2.0**(-times/t_half), ls=":", color="k", label="theory")

plt.xlabel("t (half-lives)")
plt.ylabel("number of atoms")

plt.xlim(0, n_half_lives*t_half)
plt.ylim(0,)

plt.legend(frameon=False)

plt.savefig("simple_decay.png", dpi=150)


