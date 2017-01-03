# this module is not correctly named---it's not particularly causal specific,
# but these do not appear to fit nicely anywhere else. maybe some should be
# moved to gPy.Utils?
from copy import copy
from gPy.Models import BN, CBN
from gPy.Parameters import Factor, CPT
from gPy.Data import CompactFactor, IncrementalCompactFactor
from gPy.Samplers import MultinomialSampler, BNSampler, ForwardSampler, GibbsSampler
from gPy.Graphs import ADG
from gPy.Utils import pairs, rlog
from gPy.Variables import Domain, SubDomain
from random import choice, random
import operator

_version = '$Id: LearningUtils.py,v 1.1 2008/10/07 09:11:36 jc Exp $'

def bdeu(adg,data):
    """BDeu score of the ADG a given the data"""
    score = 0.0
    for child in adg.topological_order():
        score += data.family_score(child, adg.parents(child))
    return score


def dkl(p,q):
    """KL-divergence of Q from P: D_{KL}(P||Q)"""
    p = p.copy(copy_domain=True)
    q = q.copy(copy_domain=True)
    q.common_domain(p)
    log_p = log_model(p)
    p = reduce(operator.mul, p)
    log_q = log_model(q)

    div = p*log_p - p*log_q
    # ensure 0*log 0 = 0
    for inst in p.insts():
        if p[inst] == 0:
            div[inst] = 0
    return div.z()


def log_model(p):
    """Find log P(I) for a factored distribution"""
    log_p = Factor(data=[0],domain=p)
    for p_fact in p:
        p_fact = p_fact.copy()
        p_fact.map(rlog)
        log_p += p_fact
    return log_p

class CausalWorld(SubDomain):
    """A container for experimental data derived from a causal Bayesian networks."""
    def __init__(self, bn,burnin=1000):
        """
        @param bn: A causal Bayesian network from which samples are drawn.
        @type bn: L{CBN}
        @param burnin: The burn in for the L{GibbsSampler} used for generating
        interventional data.
        @type burnin: int
        """
        super(CausalWorld,self).__init__(domain=Domain.copy(bn),variables=bn.variables())
        self._pure_model = bn.copy(copy_domain=True)
        self._pure_sampler = ForwardSampler(self._pure_model)
        self._burnin = burnin
        self._data = None
        self._inter_sampler = {}
        self._inter_data = {}

    def copy(self):
        cpy = copy(self)
        cpy._pure_model = self._pure_model.copy(copy_domain=True)
        cpy._pure_sampler = None
        cpy._inter_sampler = None
        cpy._inter_data = {}
        for k in self._inter_data.keys():
            cpy._inter_data[k] = self._inter_data[k].copy()
        # merge _data into _inter_data!
        if self._data is not None:
            cpy._data = self._data.copy()
        return cpy

    def interventions(self):
        """
        @return: A list of interventional data sets available. A zero length
        element denotes observational data.
        """
        avail_data = self._inter_data.keys()
        if self._data is not None:
            avail_data.append(frozenset())
        return avail_data

    def intervention_data(self, inter):
        """Obtain the L{IncrementalCompactFactor} corresponding to
        the intervention C{inter}. C{inter} is typically an element of
        the return value of L{interventions}. A zero length C{inter}
        indicates observational data.
        """
        if len(inter) == 0:
            return self._data
        return self._inter_data[inter]

    def observe(self, num_samples, skip=0):
        """Draw C{num_samples} observational samples, separated by C{skip}
        steps of the sampler.  These samples are appended to any existing
        observational samples and then returned.
        @return: L{IncrementalCompactFactor}
        """
        samples = self._pure_sampler.samples(num_samples,skip)
        if self._data is None:
            self._data = IncrementalCompactFactor(samples,domain=Domain())
        else:
            self._data.update(samples)
        return self._data

    def query(self, intervention, num_samples,skip=0):
        """Draw C{num_samples} interventional samples, separated by C{skip}
        steps of the sampler.  These samples are appended to any existing
        interventional samples and then returned.  The intervention made is
        that of C{intervention}.
        @param intervention: A dictionary mapping variables in the L{CBN} to
        a single value in the domain.
        @return: L{IncrementalCompactFactor}
        """
        k = frozenset(intervention.keys())
        if not self._inter_sampler.has_key(k):
            do_model = CBN.from_bn(self._pure_model.copy(copy_domain=True))
            do_model.intervene(intervention)
            self._inter_sampler[k] = GibbsSampler(do_model, self._burnin)
        do_sampler = self._inter_sampler[k]
        samples = do_sampler.samples(num_samples,skip)
        if not self._inter_data.has_key(k):
            self._inter_data[k] = IncrementalCompactFactor(samples,domain=Domain())
        else:
            self._inter_data[k].update(samples)
        return self._inter_data[k]

    def makeFactor(self, variables):
        """Construct a factor from only observations (since cannot determine
        whether interventional evidence is admissible)."""
        return self._data.makeFactor(variables)

    def makeCPT(self, child, parents, force_cpt=False, prior=1.0, check=False):
        """Use all data applicable to C{child} to make its CPT
        @param prior: the Dirichlet prior parameter (the same parameter value
        is used for all instances!)  Note there may be some problems with
        this method: a B{different} prior is used by the BDeu score. However,
        in practice, for parameter estimation, this prior method seems to be ok.
        I was lazy and it was simple to implement (cb).  If prior is zero, then
        the parameters are the maximum likelihood estimation solutions.
        """
        # child can use all observable data, and all interventional data where
        # child was not intervened.
        variables = set(parents) | set([child])
        f_child = self._data.makeFactor(variables)
        for k in self._inter_data.keys():
            if child in k:
                continue
            f = self._inter_data[k].makeFactor(variables)
            # fill in the missing zeros
            f.data_extend(dict([(var,f_child.values(var)) for var in k&f.variables()]))
            # domain monkeying
            for var in k:
                f.change_domain_variable(var, f_child.values(var))
            # eventually it looks like something edible.
            f_child += self._inter_data[k].makeFactor(variables)
            # chomp
        return CPT(f_child+prior, child, cpt_check=check, cpt_force=force_cpt)

    def family_score(self, child, parents):
        """Obtain the BDeu score of a particular family (consisting of a C{child} and
        its C{parents}) using all applicable experimental data."""
        return self.makeCPT(child, parents, prior=0.0, force_cpt=False, check=False).bdeu_score()

