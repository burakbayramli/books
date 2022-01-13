# apply the genetic algorithm to the thomson problem of finding the
# minimum energy configuration of a discrete number of charges on a
# sphere

from __future__ import print_function

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
import genetic_continuous as ga

SMALL = 1.e-16


def r_to_thetaphi(r):
    # r -> theta_0, phi_0, theta_1, phi_1, ...
    theta = np.pi*r[::2]
    phi = 2.0*np.pi*r[1::2]

    # we'll always place the first charge at the north pole, and the second
    # in the x-z plane
    theta[0] = 0.0
    phi[0] = 0.0

    phi[1] = 0.0

    return theta, phi


def thomson_cost(r):
    # we work in units so that U = sum{1/|r_i - r_j|}

    # number of charges
    nc = len(r)//2

    theta, phi = r_to_thetaphi(r)
    U = 0.0

    x = np.sin(theta)*np.cos(phi)
    y = np.sin(theta)*np.sin(phi)
    z = np.cos(theta)

    for p in range(nc):
        for q in range(p+1, nc):
            U += 1.0/np.sqrt((x[p]-x[q])**2 + (y[p]-y[q])**2 + (z[p]-z[q])**2 + SMALL)

    return U


# population size -- this is the number of chromosomes to 'compete'.
# Each chromosome has its own charge configuration
N = 20

# number of charges
n_charges = 8

# each charge has two parameters, the spherical angles
n_params = 2*n_charges

# number of generations to consider
n_generations = 10000

g = ga.GeneticAlgorithm(n_params, thomson_cost, N=N)
g.optimize(n_generations=n_generations)

# write out the final result
pb = g.p.best()

theta, phi = r_to_thetaphi(pb.r)

x = np.sin(theta)*np.cos(phi)
y = np.sin(theta)*np.sin(phi)
z = np.cos(theta)

f = open("thomson_cont_n{}.dat".format(n_charges), 'w')
f.write("# number of generations: {}\n".format(n_generations))
f.write("# minimum energy: {}\n".format(pb.cost()))

for n in range(len(x)):
    f.write("{}: {}, {}, {}\n".format(n, x[n], y[n], z[n]))

f.close()

N_hist = np.arange(len(g.U_hist))+1

plt.plot(N_hist, g.U_hist)

plt.ylabel(r"$U$", fontsize="large")
plt.xlabel("generation", fontsize="large")

ax = plt.gca()
ax.set_xscale("log")
plt.savefig("U_hist_cont_n{}.png".format(n_charges))



# fig = plt.figure()
# ax = fig.add_subplot(111, projection='3d')
# ax.scatter(x, y, z)

# plt.show()
