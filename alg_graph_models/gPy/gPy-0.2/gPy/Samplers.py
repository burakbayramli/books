"""Various samplers

@var _version: Version of this module
@type _version: String
"""

from gPy.Models import BN
import random

_version = '$Id: Samplers.py,v 1.6 2008/10/07 09:14:21 jc Exp $'

class MultinomialSampler(object):
    """
    For sampling from multinomial distributions 
    
    @ivar _cumprobs: Discrete cumulative probability distribution
    @type _cumprobs: Tuple
    @ivar _values: Values of the discrete probability distribution
    @type _values: Tuple
    """
    def __init__(self,distribution):
        """Construct a sampler for a multinomial distribution

        @param distribution: Discrete probability distribution 
        @type distribution: Dictionary
        """
        pairs = zip(distribution.values(),distribution.keys())
        pairs.sort(reverse=True)
        probs = []
        values = []
        running = 0
        for prob, value in pairs:
            running += prob
            probs.append(running)
            values.append(value)

            #to deal with numerical problems
            if running > 0.999999:
                break
        self._cumprobs = tuple(probs)
        self._values = tuple(values)

    def sample(self):
        """Draw a single sample from a multinomial distribution

        @return: A sample value from a multinomial distribution
        @rtype: Immutable
        """
        x = random.random()
        for i, cumprob in enumerate(self._cumprobs):
            if x < cumprob:
                return self._values[i]
        # for numerical reasons ...
        return self._values[i]


class BNSampler(object):
    """For sampling from Bayesian networks
    
    @ivar _cpts: For each variable, its CPT (in topological order)
    @type _cpts: Tuple
    @ivar _condition_index: Instantiated variables have their values, others have None
    @type _condition_index: List
    @ivar _dists: For each variable, a dictionary mapping parent
    instantiations to a MultinomialSampler object
    @type _dists: Tuple
    @ivar _indices: For each variable, a tuple of the indices corresponding to
    its parents
    @type _indices: Tuple
    @ivar _instskel: A list of Nones of the right length
    to hold a full joint instantiation.
    @type _instskel: List
    @ivar _indextuple: The tuple (0,1, ...n) where n is the number of variables
    @type _indextuple: Tuple
    @ivar _variables: The topological ordering of the BN's variables used
    for forward sampling
    @type _variables: Tuple
    @ivar _var2index: Maps a variable to its index in top. order
    @type _var2index: Dictionary
    """
    def __init__(self,bnm):
        variables = tuple(bnm.topological_order())
        self._variables = variables
        self._indextuple = tuple(range(len(variables)))
        self._instskel = [None] * len(variables)
        var2index = {}
        indices = self._instskel[:]
        dists = self._instskel[:]
        cpts = self._instskel[:]
        for i, variable in enumerate(variables):
            var2index[variable] = i
            cpt = bnm[variable]
            indices[i] = tuple([var2index[v] for v in sorted(cpt.parents())])
            dist = {}
            cpt_dkt = {}
            itr = cpt.parent_insts_data()
            values = sorted(cpt.values(variable))
            for parent_inst in cpt.parent_insts():
                probs = itr.next()
                dist[tuple(parent_inst)] = MultinomialSampler(dict(zip(values,probs)))
                cpt_dkt[tuple(parent_inst)] = dict(zip(values,probs))
            dists[i] = dist
            cpts[i] = cpt_dkt
        self._cpts = tuple(cpts)
        self._indices = indices
        self._dists = dists
        self._var2index = var2index
        self._condition_index = self._instskel[:]

    def condition(self,condition):
        """Make a sampler conditional on some evidence

        @param condition: Mapping from variables to their values
        @type condition: Dictionary
        """
        self._condition_index = self._instskel[:]
        for variable, value in condition.items():
            self._condition_index[self._var2index[variable]] = value
        
    def forward_sample(self):
        """Forward sample from an (unconditional) BN

        @return: A joint instantiation
        @rtype: List
        @raise KeyError: If BN has evidence
        """
        inst = self._instskel[:]
        for i in self._indextuple:
            inst[i] = self._dists[i][
                tuple([inst[j] for j in self._indices[i]])].sample()
        return inst

    def importance_sample(self):
        """Importance sample from a  BN

        @return: A joint instantiation and a weight
        @rtype: Tuple
        """
        inst = self._instskel[:]
        weight = 1
        for i, cond in enumerate(self._condition_index):
            parent_inst = tuple([inst[j] for j in self._indices[i]]) 
            if cond is None:
                inst[i] = self._dists[i][parent_inst].sample()
            else:
                inst[i] = cond
                weight *= self._cpts[i][parent_inst][cond]
        return inst, weight

        
    def rejection_sample(self):
        """Rejection sample from a  BN

        @return: A joint instantiation. If a rejection has occurred
        then the joint instantiation will contain None values
        @rtype: List
        """
        inst = self._instskel[:]
        for i, cond in enumerate(self._condition_index):
            i_value = self._dists[i][
                tuple([inst[j] for j in self._indices[i]])].sample()
            if cond is not None and cond != i_value:
                break
            inst[i] = i_value
        return inst

    def uncondition(self):
        """Remove all evidence 
        """
        self._condition_index = self._instskel[:]
        
    def variable_index(self,variable):
        """Return the index for a variable in joint instantiations

        @return: The index for a variable in joint instantiations
        @rtype: Integer
        """
        return self._var2index[variable]

    def variables(self):
        """Return the topological ordering of variables in joint instantiations

        @return: The ordering of variables in joint instantiations
        @rtype: Tuple
        """
        return self._variables


class GibbsSampler(object):
    """

    DO NOT USE: this is INCORRECT
    
    A Gibbs sampler for Bayesian networks.  Variables are instantiated in a
    topological order. Hence when no variables are instantiated, this is just
    forward sampling (no burn in is necessary).

    When conditioning occurs, topological ordering is still used but conditioned
    variables are not updated.
    """
    def __init__(self, bn, burnin=1000):
        if not isinstance(bn, BN):
            raise ValueError,'only works with Bayesian network'
        self._inst = {}
        self._samplers = {}
        self._parents = {}
        self._values = {}
        self._variables = sorted(bn.variables())
        for child in self._variables:
            self._values[child] = tuple(sorted(bn.values(child)))
            ids = range(len(self._values[child]))
            if len(ids) == 1:
                self._values[child] = tuple(bn.values(child))
                self._inst[child] = ids[0]
                continue
            cpt = bn[child]
            self._samplers[child] = {}
            self._parents[child] = sorted(cpt.variables() - frozenset([child]))
            for inst in cpt.insts(self._parents[child]):
                dist = {}
                var_inst = dict(zip(self._parents[child], inst))
                for id, val in zip(ids,self._values[child]):
                    var_inst[child] = val
                    dist[id] = cpt[var_inst]
                self._samplers[child][inst] = MultinomialSampler(dist)
            self._inst[child] = random.choice(ids)

        self._toporder = tuple(bn.topological_order())
        for x in xrange(burnin):
            self._next_sample()

    def sample(self):
        self._next_sample()
        return tuple([self._values[child][self._inst[child]] for child in self._variables])

    def _next_sample(self):
        #for child in self._samplers.keys():
        for child in self._toporder:
            if child not in self._samplers:
                continue
            subinst = tuple([self._values[var][self._inst[var]] for var in self._parents[child]])
            self._inst[child] =  self._samplers[child][subinst].sample()

    def samples(self, num, skip=0):
        records = []
        for i in xrange(num):
            for j in xrange(skip):
                self._next_sample()
            self._next_sample()
            records += [tuple([self._inst[var] for var in self._variables])]

        contig = _histogram(records)
        records = [record + (contig[record],) for record in contig.keys()]
        return (self._variables, self._values, self._variables, records)

def _histogram(samples):
    hist = {}
    for sample in samples:
        sample = tuple(sample)
        if sample in hist:
            hist[sample] += 1
        else:
            hist[sample] = 1
    return hist

class ForwardSampler(GibbsSampler):
    """A forward sampler for an unconditional Bayesian networks."""
    def __init__(self, bn):
        super(ForwardSampler, self).__init__(bn, burnin=0)

class MetropolisSampler(object):
    """An abstract Metropolis-Hastings sampler. L{_mutate} and L{_accept} must be
       overridden."""
    def __init__(self, seed, burnin=1000):
        super(MetropolisSampler,self).__init__()
        self._sample = seed

        for i in xrange(burnin):
            self._next_sample()

    def sample(self, skip=0):
        """Obtain one sample, separated from the last by C{skip} steps"""
        for i in xrange(skip):
            self._next_sample()
        self._next_sample()
        return self._sample

    def samples(self, num_samples, skip=0):
        """Obtain C{num_samples} sample, each separated by C{skip} steps"""
        samples = [None for i in xrange(num_samples)]
        for i in xrange(num_samples):
            for j in xrange(skip):
                self._next_sample()
            self._next_sample()
            samples[i] = self._sample.copy()
        return samples

    def _next_sample(self):
        sample = self._mutate()
        if random.random() < self._accept(sample):
            self._sample = sample

    def _mutate(self):
        """generate to a neighbour of the current sample and the proposal distribution"""
        return self._sample

    def _accept(self,sample):
        """log probability of acceptance"""
        return 0.0

