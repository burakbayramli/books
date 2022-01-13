# Compute the volume of a hypersphere via Monte Carlo integration

import numpy as np
import scipy.special as special

def vol(d, N=1000000):
    """ d is the dimensionality, N is the number of samples """

    v = 0

    for n in range(N):
        # a point in d-dimensional space
        r = np.random.uniform(-1.0, 1.0, d)
    
        R = np.sqrt(np.sum(r**2))

        if R <= 1.0:
            v += 1

    # we normalize by the volume of our domain, which in our
    # case is just [-1,1]**d
    V = 2.0**d

    return v*(V/N)



d = 10

print("{}-d hypersphere volume:".format(d))

for N in [10, 100, 1000, 10000, 100000, 1000000]:
    print("N: {:8d}, V = {}".format(N, vol(d, N=N)))

# volume of a unit hypersphere
analytic = np.pi**(d/2)/special.gamma(d/2 + 1.0)

print("analytic value = {}".format(analytic))


