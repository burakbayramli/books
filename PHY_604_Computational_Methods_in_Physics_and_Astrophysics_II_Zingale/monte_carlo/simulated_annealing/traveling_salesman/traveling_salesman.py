# Solve the traveling salesman problem using simulated annealing

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import random
import sys

class City(object):
    # a city on our map

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return "({:5.3f}, {:5.3f}) ".format(self.x, self.y)


class Tour(object):
    # an ordered collection of cities that we will travel to

    def __init__(self, cities):
        # cities is a list of City objects

        self.cities = cities
        self.old_i = None
        self.old_j = None

    def swap(self):
        # swap 2 cities

        i = random.randrange(len(self.cities))
        j = random.randrange(len(self.cities))

        while i == j:
            j = random.randrange(len(self.cities))

        self.old_i = i
        self.old_j = j

        self.cities[i], self.cities[j] = self.cities[j], self.cities[i]

    def unswap(self):
        # undo the swapping
        self.cities[self.old_j], self.cities[self.old_i] = self.cities[self.old_i], self.cities[self.old_j]


    def distance(self):
        # compute the distance of the tour
        N = len(self.cities)
        d = 0.0
        for n in range(1, N):
            d += np.sqrt((self.cities[n].x - self.cities[n-1].x)**2 +
                         (self.cities[n].y - self.cities[n-1].y)**2)

        # complete the circuit
        d += np.sqrt((self.cities[0].x - self.cities[N-1].x)**2 +
                     (self.cities[0].y - self.cities[N-1].y)**2)

        return d

    def plot(self, filename):
        plt.clf()
        xs = [q.x for q in self.cities]
        ys = [q.y for q in self.cities]

        # complete the circuit
        xs += [self.cities[0].x]
        ys += [self.cities[0].y]

        plt.scatter(xs, ys)
        plt.plot(xs, ys)
        plt.title("distance = {}".format(self.distance()))

        plt.savefig(filename, dpi=150)

    def __str__(self):
        s = ""
        for c in self.cities:
            s += "{}".format(c)
        return s


def optimize_route(cities, do_plot=True):
    """ given a list of City objects, optimize the route connecting them """

    # now randomize the seed -- calling with no argument will use system time
    random.seed()

    # create the Tour
    tour = Tour(cities)

    # make an initial plot
    tour.plot("initial.png")

    d_old = tour.distance()

    print("initial_distance = {}".format(d_old))

    Tmin = 1.e-3
    Tmax = 10.0
    tau = 1.e4

    # for storing the history
    Ts = []
    ds = []

    # cooling loop
    t = 0
    T = Tmax

    while T > Tmin:

        Ts.append(T)
        ds.append(d_old)

        T = Tmax*np.exp(-t/tau)

        # swap two cities
        tour.swap()

        # get the new distance
        d = tour.distance()

        # check whether we should keep it
        if random.random() > np.exp(-(d - d_old)/T):
            # reject
            tour.unswap()
        else:
            d_old = d

        t += 1

    print("final distance = {}".format(d_old))

    if do_plot:
        tour.plot("final.png")

        plt.clf()
        plt.plot(Ts, ds)
        plt.xlabel("T(t)")
        plt.ylabel("d")

        ax = plt.gca()
        ax.set_xscale("log")
        ax.invert_xaxis()

        plt.savefig("traveling_salesman_hist.png", dpi=150)

    return tour


if __name__ == "__main__":

    # number of cities
    N = 50

    # initialize the cities with random positions -- we use a seed here
    # to always get the same city configuration for when running multiple
    # times
    random.seed(1010)
    cities = []
    for n in range(N):
        cities.append(City(random.random(), random.random()))

    optimize_route(cities)

