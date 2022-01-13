# perform radioactive decay with a transformed random number generator.

import numpy as np
import random
import matplotlib.pyplot as plt

def my_ran(mu):

    # random.random() gives us a number in [0,1)
    z = random.random()

    # the transformation x = -1/mu ln(1 - z) gives us
    # a random number that obeys the probability distribution
    # p(x)dx = mu e**(-mu x) dx
    return -np.log(1.0 - z)/mu


N = 1000

# create a list that stores how long each atom lived
atom_life = []

t_half = 1.0
mu = np.log(2)/t_half

for n in range(N):
    atom_life.append(my_ran(mu))

atom_life = np.sort(np.array(atom_life))

n_half_lives = 5

times = np.linspace(0.0, n_half_lives*t_half, 100)

N_decayed = []
for t in times:
    try: q = np.argwhere(atom_life <= t)[-1][0]
    except: q = 0

    N_decayed.append(q)

plt.scatter(times, np.array(N_decayed), s=10, marker="x")
plt.plot(times, N - N*2.0**(-times/t_half), ls=":", color="k")

plt.xlabel("t (half-lives)")
plt.ylabel("number of atoms decayed")

plt.xlim(0, n_half_lives*t_half)
plt.ylim(0,)

plt.savefig("transform_decay.png")



