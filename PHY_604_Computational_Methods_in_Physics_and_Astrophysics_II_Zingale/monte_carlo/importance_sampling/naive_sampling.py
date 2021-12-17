# We want to compute
#
# I = int_0^1 x^{-1/2} / (exp(x) + 1) dx
#
# here we just use uniform random numbers -- we'll have problems near
# the origin, and our answer will be unreliable

import random
import matplotlib.pyplot as plt
import numpy as np

def f(x):
    return x**(-1.0/2.0)/(np.exp(x) + 1.0)


# plot our function
eps = 1.e-16
x = np.linspace(0.0+eps, 1.0, 1000)
plt.plot(x, f(x))
plt.ylim(0,10)
plt.xlabel("x")
plt.ylabel("f(x)")
plt.savefig("integrand-importance.png", dpi=150)



# do multiple runs
for t in range(5):

    I = 0.0
    N = 10000

    for n in range(N):
        x = np.random.uniform()
        I += f(x)

    I *= 1.0/N

    print("N: {}, I = {}".format(N, I))
