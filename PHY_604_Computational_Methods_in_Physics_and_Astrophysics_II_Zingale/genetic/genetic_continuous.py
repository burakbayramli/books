""" implement the continuous geneatic algorithm """

from __future__ import print_function

import numpy as np
import random

class Chromosome(object):
    """an individual chromosome for one organism in our population.  This
    will consist of n_params represented as a real array (this is a
    continuous algorithm).  A chromosome will also keep track of its
    cost, according to the fitness function

    """

    def __init__(self, n_params, fit_func):
        """ randomly initialize the parameters """
        self.n_params = n_params
        self.fit_func = fit_func

        # our parameters is the chromosome
        self.r = np.zeros(n_params)

    def random_initialize(self):
        """ create our chromosome of random parameters """
        self.r[:] = np.random.random(size=self.n_params)

    def cost(self):
        """compute the cost (according to the fitness function) of this
        organism"""
        return self.fit_func(self.r)

    def __lt__(self, other):
        # we use the fitness function to provide the ordering
        return self.cost() < other.cost()

    def __str__(self):
        return "{}".format(self.cost())

class Population(object):
    """Create a population of chromosomes for our problem

    we assume that our parameters are in [0, 1)

    Our convection is that the lower the fitness the better.
    """

    def __init__(self, N, n_params, fit_func):
        """ Create the initial population """
        self.N = N

        # we'll create 2N chromosomes, sort them according to the
        # fitness function, and then take the N best
        t_pop = []
        for n in range(2*N):
            c = Chromosome(n_params, fit_func)
            c.random_initialize()
            t_pop.append(c)

        # sorting orders in terms of the fitness function
        t_pop.sort()
        self.population = t_pop[0:self.N]

        self.generation = 0

    def best(self):
        """ return the best (fittest) chromosome """
        self.population.sort()
        return self.population[0]

    def select_parents(self):
        """pick the chromosomes from the gene pool that will reproduce.  We do
        this by running a tournamenet, randomly picking pairs as
        potential parents and keeping the fittest of the pair

        """
        _tmp = list(self.population)
        random.shuffle(_tmp)

        parents = []
        for n in range(self.N//2):
            p1 = _tmp.pop()
            p2 = _tmp.pop()
            if p1 < p2:
                parents.append(p1)
            else:
                parents.append(p2)

        self.parents = parents

    def crossover(self):
        """Create our children via crossover on our parents.  We randomly
        select the parents from the pool of parents and use a fixed
        crossover point (in array index space) to create the
        children

        """

        # here we'll restrict the breeding to the fittest N/2 parents,
        # but other variations exist
        p = list(self.parents)
        p.sort()
        p = p[0:self.N//2]
        random.shuffle(p)

        children = []

        for n in range(len(p)//2):
            p1 = p.pop()
            p2 = p.pop()

            c1 = Chromosome(p1.n_params, p1.fit_func)
            c2 = Chromosome(p1.n_params, p1.fit_func)

            c1.r[0:p1.n_params//2] = p1.r[0:p1.n_params//2]
            c1.r[p1.n_params//2:] = p2.r[p1.n_params//2:]

            children.append(c1)

            c2.r[0:p1.n_params//2] = p2.r[0:p1.n_params//2]
            c2.r[p1.n_params//2:] = p1.r[p1.n_params//2:]

            children.append(c2)

        # we now have the new generation
        self.generation += 1

        self.population = self.parents + children
        self.population.sort()

    def mutation(self, probability=0.05, num_elite=2):
        """mutate a fraction of the bits in the chromosome randomly.  For the
        num_elite best chromosomes, we only keep a mutation if it
        improves the cost

        """

        # we assume we are sorted coming into here
        for n, c in enumerate(self.population):
            if n < num_elite:
                # store the old
                t = c.r.copy()
                cost_old = c.cost()

            # randomly flip bits
            for b in range(c.n_params):
                if random.random() < probability:
                    c.r[b] = random.random()

            cost = c.cost()

            # for elite configurations, reset if the new cost is higher
            if n < num_elite:
                if cost > cost_old:
                    c.r[:] = t[:]

        # resort
        self.population.sort()

    def __str__(self):
        s = ""
        for n in range(self.N):
            p = self.population[n]
            s += "{}: {}\n".format(n, p.cost())

        return s


class GeneticAlgorithm(object):
    """ a driver for the continuous genetic algorithm """

    def __init__(self, n_params, cost_function, N=20):
        self.n_params = n_params
        self.cost_function = cost_function
        self.N = N
        self.U_hist = []

        self.p = Population(self.N, self.n_params,
                            self.cost_function)

    def optimize(self, n_generations=2000):
        """run the genetic algorithm for multiple generations"""
        for g in range(n_generations):
            self.p.select_parents()
            self.p.crossover()
            self.p.mutation()

            U = self.p.best().cost()
            self.U_hist.append(U)
            print("{}: {}".format(g, U))
