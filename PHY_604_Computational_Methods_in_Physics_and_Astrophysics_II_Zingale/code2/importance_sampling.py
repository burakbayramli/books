# We want to compute
#
# I = int_0^1 x^{-1/2} / (exp(x) + 1) dx
#
# we use importance sampling, drawing on a distribution of random
# numbers that removes the x^{-1/2} term

import random
import matplotlib.pyplot as plt
import numpy as np

def my_ran():

    # random.random() gives us a number in [0,1)
    z = random.random()

    # the transformation x = x**2 gives us a random number that
    # obeys the probability distribution p(x) dx = 1/(2 sqrt(x)) dx
    return z**2



# check our random number distribution via a plot
r = []
for n in range(1000000):
    r.append(my_ran())

plt.clf()
plt.hist(r, normed=True, bins=20)

x = np.linspace(1.e-5, 1.0, 100)
plt.plot(x, 1.0/(2.0*np.sqrt(x)), lw=2, color="r")

ax = plt.gca()

ax.set_yscale('log')

plt.savefig("ran_dist.png")



# do multiple runs:
for n in range(5):
    I = 0.0
    N = 100000

    for n in range(N):
        x = my_ran()
        I += 1.0/(np.exp(x) + 1.0)

    I *= 2.0/N

    print("N: {}, I = {}".format(N, I))

