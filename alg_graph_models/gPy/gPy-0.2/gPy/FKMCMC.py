from gPy.Samplers import MetropolisSampler
from gPy.Utils import logsumexp, binom, negative_infinity, pairs, rlog, powersetn, TimedOut
from math import exp,log
from random import randrange, random, shuffle
import operator
import sys, traceback

_version = '$Id: FKMCMC.py,v 1.2 2008/10/07 09:09:43 jc Exp $'

# MCMC on orders
# (based on Friedman & Koller, 2000: Being Bayesian about Network Structure)
class Order(object):
    def __init__(self, family_cache, order, score = None, order_size = None):
        self._score = score
        self._family_cache = family_cache
        self.order = order
        if order_size is None:
            self.order_size = len(order)
        else:
            self.order_size = order_size

    def copy(self):
        return Order(self._family_cache, self.order[:], self._score, self.order_size)

    def flip(self):
        # flip two nodes.
        # i  j
        #  \/
        #  /\
        # i  j:
        # if i < j, then after the flip, the element at i is parent of fewer
        # children than before, whereas the element at j has more, and these
        # are the only changes. this makes score calculation more efficient.

        i = randrange(0,self.order_size)
        j = randrange(0,self.order_size-1)
        if j >= i:
            j += 1
        if i > j:
            i, j = j, i
        #prev = self.copy()
        self.order[i], self.order[j] = self.order[j], self.order[i]
#       causes overflows. FIXME!
#        if self._score is not None:
#            score_config = self._family_cache.family_score
#            for k in xrange(self.order_size):
#                child = self.order[k]
#                if k == i:
#                    pk = j
#                elif k == j:
#                    pk = i
#                else:
#                    pk = k
#                child_delta = [score_config(child, pparents)
#                                for pparents in prev.parent_sets(pk)
#                                    if prev.order[i] in pparents]
#                self._score -= logsumexp(child_delta)
#                child_delta = [score_config(child,parents) for parents in self.parent_sets(k) if self.order[i] in parents]
#                self._score += logsumexp(child_delta)
        self._score = None

    def deckcut(self):
        # cut the deck:
        i = randrange(0,self.order_size)
        self.order[i:], self.order[:i] = self.order[:i], self.order[i:]
        self._score = None

    def parent_sets(self, child_index):
        # a parent set is a subset of all nodes preceding the child in the order
        before_child = self.order[:child_index]
        child = self.order[child_index]
        use_best_families, families = self._family_cache.best_families(child, before_child)
        if use_best_families:
            return families

        parents = frozenset(before_child) & self._family_cache.potential_parents(child)
        return powersetn(parents, self._family_cache._max_parents_family)

    def score(self):
        if self._score is not None:
            return self._score

        # find log P(D | <) (taking logs for numerical stability)
        self._score = 0
        score_config = self._family_cache.family_score
        for i in xrange(self.order_size):
            child = self.order[i]
            child_scores = [score_config(child,parents) for parents in self.parent_sets(i)]
            # gPy uses the log of the score. this makes sense when marginalising over
            # structures. however, friedman & koller have:
            # P(D | <) = \prod_i \sum_U score(X_i, U | D)
            # where i \in 1,..,N, U \in U < X_i, |U| <= k
            # so we must watch out for numerical instability
            self._score += logsumexp(child_scores)

        # P(< | D) = P(D | <) P(<) / P(D)
        # P(D) = \sum_< P(D | <) P(<)
        # < ~ Uniform => P(<) terms cancel
        # hence P(< | D) = Z^{-1} P(D | <) where Z = \sum_< P(D | <)
        # Z does not help us discriminate between the scores and so we
        # do not compute it to score an order (assuming D is fixed).
        # hence this function returns log P(D | <)
        return self._score

    def parents_contains_score(self, parents, child):
        # log P(parents \in Pa(child) | <, D) = log sum_{U : parents subseteq U} score(X,U | U) -
        #                                       log sum_U score(X,U | D)
        parents = frozenset(parents)

        # for sets, in python, not(a <= b) != a > b
        if not (parents <= self._family_cache.potential_parents(child)):
            return negative_infinity

        scores, edge_scores = [], []
        for family_parents in self.parent_sets(self.order.index(child)):
            score = self._family_cache.family_score(child, family_parents)
            if parents <= set(family_parents):
                edge_scores.append(score)
            scores.append(score)

        if not edge_scores:
            return negative_infinity

        return logsumexp(edge_scores) - logsumexp(scores)

    def conditional_probability(self, child, family_evidence):
        # log P(x_k | w_k) = log sum_U P(Pa(child) = U) score(X,U | evidence)
        cond = []
        for family_parents in self.parent_sets(self.order.index(child)):
            score = self._family_cache.family_score(child, family_parents)
            cond.append(score + self.parents_score(child, family_parents))
        return logsumexp(cond)

    def parents_score(self, child, parents):
        # log P(Pa(child) = parents | <, D) = log score(X, parents | parents) -
        #                                     log sum_U score(X,U | D)
        parents = frozenset(parents)
        if parents not in self._family_cache.potential_parents(child):
            return negative_infinity
        score = self._family_cache.family_score(child, parents)
        scores = []
        for family_parents in self.parent_sets(self.order.index(child)):
            scores.append(self._family_cache.family_score(child, family_parents))

        return score - logsumexp(scores)

    def arrow_score(self, parent, child):
        # log P(parent -> child | <, D) = log sum_{U : parent in U} score(child,U | D) -
        #                                 log sum_U score(child,U | D)
        return self.parents_contains_score([parent], child)

    def markov_blanket_score(self, xi, xj):
        # P(xi ~m~ xj | <, D)
        # NOTE: this is not the log!
        markov_scores = [1]
        for xk in xrange(self.order.index(xj)):
            child = self.order[xk]
            markov_scores.append(1 - exp(self.parents_contains_score([xi,xj], child)))
        return 1 - (1 - exp(self.arrow_score(xi, xj)))*reduce(operator.mul, markov_scores)


class FamilyScoreCache(object):
    def __init__(self, data, variables,
                       max_potential_parents = None, max_parents_family = None,
                       max_best_families = None, best_family_scale = None):
        # maximum number of parents to consider for each variable (based on
        # edge score)
        self._max_potential_parents = max_potential_parents
        # maximum number of parents in each family (< max_potential_parents!)
        self._max_parents_family = max_parents_family
        # maximum number of families to store
        self._max_best_families = max_best_families
        # log of the scale factor by which the best electable family must be
        # better than the worst best family
        self._best_family_scale = best_family_scale
        self._variables = frozenset(variables)
        self._family_priors = {}
        self.flush(data)

    def flush(self, data):
        self._scores = {}
        self._data = data
        self._update_potential_parents()

    def from_adg(self, adg):
        for child in self._variables:
            self._potential_parents[child] = frozenset(adg.parents(child))

    def family_score(self, child, parents):
        k = tuple([child,frozenset(parents)])
        if k in self._scores:
            return self._scores[k]

        self._scores[k] = self._data.family_score(child, k[1])
        self._scores[k] += self.family_prior(len(parents))
        return self._scores[k]

    def mutual_information(self, variables):
        """Find the mutual information amongst variables by MLE on given data.
        """

        log_marginals = []
        for variable in variables:
            log_marginals.append(self._data.makeFactor([variable]))
            z = log(log_marginals[-1].z())
            log_marginals[-1].map(lambda x: rlog(x) - z)

        joint = self._data.makeFactor(variables)
        log_joint = joint.copy().map(rlog)
        jz = joint.z()
        joint /= jz
        joint *= log_joint - rlog(jz) - reduce(operator.add, log_marginals)
        return joint.z()

    def _update_potential_parents(self):
        """before any MCMC step, set the possible parents"""
        self._potential_parents = {}
        self._best_families = {}
        if self._max_potential_parents is None:
            self._best_family_scale = None
        for child in self._variables:
            # each variable is restricted to max_potential_parents parents where
            # the score of the edge, parent -> child, is used to order parents.
            if self._max_potential_parents is None:
                self._potential_parents[child] = self._variables - frozenset([child])
                continue
            score_parent = {}
            for parent in self._variables:
                if parent == child:
                    continue
                # XXX: is this correct? FK seems rather vague on this point.
                #score_parent[parent] = self.family_score(child, [parent])
                # Tong's thesis suggests this:
                score_parent[parent] = self.mutual_information([child,parent])
            parents = sorted(score_parent.keys(), cmp=lambda x, y: cmp(score_parent[y], score_parent[x]))
            self._potential_parents[child] = frozenset(parents[:self._max_potential_parents])
            if self._best_family_scale is not None:
                self._update_best_families(child)

    def _update_best_families(self, child):
        # calculate the best families for a child.
        # the best families are those that score highest; keep the top max_parents_family
        # families.
        family_score = {}
        for parents in powersetn(self._potential_parents[child], self._max_parents_family):
            family_score[frozenset(parents)] = self.family_score(child, parents)

        family_order = sorted(family_score.keys(), cmp=lambda x,y: cmp(family_score[y], family_score[x]))
        family_order = family_order[:self._max_best_families]
        self._best_families[child] = tuple([(parents,family_score[parents]) for parents in family_order])

    def potential_parents(self, child):
        return self._potential_parents[child]

    def best_families(self, child, order_parents):
        if self._best_family_scale is None:
            return False, None

        common_parents = frozenset(order_parents) & self._potential_parents[child]
        best_families = []
        best_score = None
        for best_parents, score in self._best_families[child]:
            if best_parents <= common_parents:
                best_families.append(best_parents)
                if best_score is None or score > best_score:
                    best_score = score
        if best_score is None:
            return False, None
        if best_score - self._best_family_scale < self._best_families[child][-1][1]:
            return False, None
        return True, best_families

    def variables(self):
        return self._variables

    def family_prior(self, num_parents):
        """return the unnormalised log family prior"""
        if num_parents in self._family_priors:
            return self._family_priors[num_parents]

        # family prior is prop. to:
        # (  n-1  )^{-1}
        # ( |PaG| )
        # we don't normalise as we are lazy and the normalisation
        # constant always cancels.
        family_prior = -float(log(binom(len(self._variables) - 1, num_parents)))
        if self._max_parents_family is not None:
            self._family_priors[num_parents] = family_prior
        return family_prior

class OrderSampler(MetropolisSampler):
    def __init__(self,seed,family_cache,p_flip=0.1,burnin=1000):
        """seed is a sequence of variables, p_flip is
        probability of chosing flip over deck-cutting
        """
        self._p_flip = p_flip
        self._family_cache = family_cache
        seed = list(seed)
        shuffle(seed)
        super(OrderSampler,self).__init__(Order(self._family_cache, seed), burnin)

    def _mutate(self):
        # two choices (Friedman & Koller, 2000):
        #  (1) flip two nodes in the order
        #  (2) cutting the deck -- swap the first half with the last half
        # the above paper picks between each with some probability p,
        # since (1) takes small steps and so is slow to mix, whereas (2)
        # is more expensive to compute.
 
        sample = self._sample.copy()
        if random() < self._p_flip:
            sample.flip()
        else:
            sample.deckcut()
        return sample

    def _accept(self,sample):
        # accept with probability:
        # min[ 1, P(<' | D) q(< | <') ]
        #         -------------------
        #         P(< | D)  q(<' | <)
        # with the above operators, q(< | <') == q(<' | <)
        # hence we must find P(<' | D)/P(< | D)
        try:
            return min(1, exp(sample.score() - self._sample.score()))
        except OverflowError:
            print 'overflow!'
            if sample.score() - self._sample.score() < -600:
                return 0
            else:
                return 1

# a collection of order samplers, sharing the same score cache.
class OrderEnsemble(object):
    def __init__(self, data, variables, num_orderings, p_flip=0.5, burnin=1500,
                                  max_potential_parents = 20, max_parents_family = 3,
                                  max_best_families = 4000, best_family_scale=log(10)):
        self._family_cache = FamilyScoreCache(data, variables, max_potential_parents,max_parents_family,
                                              max_best_families, best_family_scale)
        self._orders = []
        for i in xrange(num_orderings):
            self._orders.append(OrderSampler(variables, self._family_cache, p_flip, burnin))

    def sample(self, skip=0):
        return [order.sample(skip) for order in self._orders]

class OrderGraphSearch(object):
    def search(self, data, order):
        pass

def fk_exp_graph(graph_search, data, num_orderings=10, num_samples=10, sample_every=10, burnin=1000, max_parents_family=3):
    graphs = []
    ensemble = OrderEnsemble(data, data.variables(), num_orderings=num_orderings, burnin=burnin, max_parents_family=max_parents_family)
    for i in xrange(num_samples):
        esample = ensemble.sample(skip=sample_every)
        try:
            graphs += [graph_search.search(data, sample.order) for sample in esample]
        except TimedOut:
            raise
        except:
            print >>sys.stderr, 'skipped!'
            traceback.print_exc(file=sys.stderr)
            continue

    return graphs

# try:
#     import psyco

#     psyco.bind(Order)
#     psyco.bind(FamilyScoreCache)
#     psyco.bind(OrderSampler)
#     psyco.bind(OrderEnsemble)
#     psyco.bind(fk_exp_graph)
# except:
#     pass

