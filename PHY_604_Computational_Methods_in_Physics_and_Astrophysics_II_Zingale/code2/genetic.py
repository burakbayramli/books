from __future__ import print_function

import numpy as np
import random

class Chromosome(object):
    # an individual chromosome for one organism in our population this
    # will consist of n_params each encoded into n_genes 0s and 1s
    # (so n_genes is the number of bits allocated to each parameter)
    # a chromosome will also keep track of its cost, according
    # to the fitness function

    def __init__(self, n_params, n_genes, fit_func):
        # randomly initialize the parameters and encode them
        self.n_params = n_params
        self.fit_func = fit_func
        self.n_genes = n_genes

        self.clen = n_params * n_genes

        self.r = np.zeros(n_params)
        self.chromosome = np.zeros((self.clen), dtype=np.bool)

    def random_initialize(self):
        # create our chromosome of random parameters
        self.r = np.random.random(size=self.n_params)
        self.encode()

    def encode(self):
        # encode the current set of parameters into the genes
        # (bits) that make up our chromosome
        y = np.zeros((len(self.r), self.n_genes), dtype=np.bool)

        for n in range(len(self.r)):
            r = self.r[n]
            y[n,0] = bool(round(r))
            for j in range(1, self.n_genes):
                tmp = np.sum(2**(j-np.arange(j)-1)*y[n,:j])
                y[n,j] = bool(round(2**j*r - tmp))

        # when we flatten y, it will be done in row-major order,
        # so the individual parameters will be together in the
        # chromosome
        self.chromosome = y.flatten()

    def decode(self):
        # decode the chromosome into the vector of parameters

        y = self.chromosome.copy().reshape(len(self.r), self.n_genes)

        for n, p in enumerate(y):
            self.r[n] = np.sum(p/2.0**(np.arange(len(p))+1))

    def cost(self):
        self.decode()
        return self.fit_func(self.r)

    def __lt__(self, other):
        # we use the fitness function to provide the ordering
        self.decode()
        other.decode()
        return self.cost() < other.cost()

    def __str__(self):
        return "{}".format(self.cost())

class Population(object):
    # create a population of chromosomes for our problem Each
    # chromosome will consist of a binary encoding of n_params
    # parameters into n_genes genes (0 or 1)
    #
    # we assume that our parameters are in [0, 1)
    #
    # Our convection is that the lower the fitness the better

    def __init__(self, N, n_params, n_genes, fit_func):
        # create the initial population
        self.N = N

        # we'll create 2N chromosomes, sort them according to the
        # fitness function, and then take the N best
        t_pop = []
        for n in range(2*N):
            c = Chromosome(n_params, n_genes, fit_func)
            c.random_initialize()
            t_pop.append(c)

        # sorting orders in terms of the fitness function
        t_pop.sort()
        self.population = t_pop[0:self.N]

        self.population_count = 0

    def best(self):
        self.population.sort()
        return self.population[0]

    def select_parents(self):
        # pick the chromosomes from the gene pool that will reproduce
        # we do this by running a tournamenet, randomly picking pairs
        # as potential parents and keeping the fittest of the pair
        _tmp = list(self.population)
        random.shuffle(_tmp)

        parents = []
        for n in range(0, len(_tmp), 2):
            p1 = _tmp.pop()
            p2 = _tmp.pop()
            if p1 < p2:
                parents.append(p1)
            else:
                parents.append(p2)

        self.parents = parents

    def crossover(self):
        # create our children via crossover on our parents we randomly
        # select the parents from the pool of parents and use a fixed
        # crossover point to create the children

        # here we'll restrict the breeding to the fittest N/2 parents,
        # but other variations exist
        p = list(self.parents)
        p.sort()
        p = p[0:self.N//2]
        random.shuffle(p)

        children = []

        for n in range(0, len(p), 2):
            p1 = p.pop()
            p2 = p.pop()

            c1 = Chromosome(p1.n_params, p1.n_genes, p1.fit_func)
            c2 = Chromosome(p1.n_params, p1.n_genes, p1.fit_func)

            c1.chromosome[0:p1.clen//2] = p1.chromosome[0:p1.clen//2]
            c1.chromosome[p1.clen//2:] = p2.chromosome[p1.clen//2:]

            children.append(c1)

            c2.chromosome[0:p1.clen//2] = p2.chromosome[0:p1.clen//2]
            c2.chromosome[p1.clen//2:] = p1.chromosome[p1.clen//2:]

            children.append(c2)

        # we now have the new generation
        self.population_count += 1

        self.population = self.parents + children
        self.population.sort()

    def mutation(self, probability=0.05, num_elite=2):
        # mutate a fraction of the bits in the chromosome randomly
        # For the num_elite best chromosomes, we only keep a mutation
        # if it improves the cost

        # we assume we are sorted coming into here
        for n, c in enumerate(self.population):
            if n < num_elite:
                # store the old
                t = c.chromosome.copy()
                cost_old = c.cost()

            # randomly flip bits
            for b in range(c.clen):
                if random.random() < probability:
                    c.chromosome[b] = not c.chromosome[b]

            cost = c.cost()

            # for elite configurations, reset if the new cost is higher
            if n < num_elite:
                if cost > cost_old:
                    c.chromosome[:] = t[:]

        # resort
        self.population.sort()

    def __str__(self):
        s = ""
        for n in range(self.N):
            p = self.population[n]
            s += "{}: {}\n".format(n, p.cost())

        return s


class GeneticAlgorithm(object):
    """ a driver for the genetic algorithm """

    def __init__(self, n_params, n_genes, cost_function, N=20):
        self.n_params = n_params
        self.n_genes = n_genes
        self.cost_function = cost_function
        self.N = N

    def optimize(self, n_generations=2000):
        self.U_hist = []
        self.p = Population(self.N, self.n_params,
                            self.n_genes, self.cost_function)

        for g in range(n_generations):
            self.p.select_parents()
            self.p.crossover()
            self.p.mutation()

            U = self.p.best().cost()
            self.U_hist.append(U)
            print("{}: {}".format(g, U))
