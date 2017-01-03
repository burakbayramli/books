# based on:
# -- Constraint-based Structural Learning in Bayesian Networks using Finite Data Sets, H. Steck, PhD Thesis (2001)
# -- Causality, Pearl (2000)
# -- Causation, Prediction, and Search, Spirtes, Glymour, and Sheines (2000)
# -- An Algorithm for Deciding if a Set of Observed Independencies Has a Causal Explanation, Verma & Pearl (1992)
# -- Functional Connectivity Modelling in FMRI Based On Causal Networks, Delens, De Mazi\`re & Van Hulle
# also based indirectly on the work of Spirtes, Glymour and Scheines (no paper used).
#
from gPy.Graphs import ADG, UGraph, EssentialGraph
from gPy.FKMCMC import OrderGraphSearch
from gPy.Utils import subsetn, pairs, rlog, powerset, swap, is_finite, square
from gPy.gPyC import chisqprob
from math import log

_version = '$Id: PC.py,v 1.1 2008/10/07 09:12:13 jc Exp $'

class ICPatternFail(Exception):
    pass

# a pattern is a semi-markov graph; some edges are left undirected.
class ICPattern(EssentialGraph):
    def __init__(self, ci=None, constrain_order=None):
        """Given the exhaustive set of all conditional independences, infer the underlying
        causal graph up to d-separation equivalence.
        @param ci: conditional independences
        @type ci: CI
        @return: pattern of the conditional independences
        @rtype: Graph containing directed and undirected edges
        """
        if ci is None:
            # copy construct
            super(ICPattern, self).__init__()
            return

        super(ICPattern, self).__init__(vertices=ci.variables())

        # fix the order
        self.constrain_order(constrain_order)

        # begin with complete undirected graph
        self.complete(self.vertices())

        # remove all independencies
        self._remove_indeps(ci)

        # find the immoralities
        self._find_colliders(ci)

        # impose the order constraint on remaining edges (if any)
        self._orient_from_order()

        # resolve through graphical deduction to a Markov Equivalence class
        self.resolve()

    def _remove_indeps(self,ci):
        # for each pair, test a _|_ b | s, for any s.
        for (a,b) in pairs(ci.variables()):
            if ci.has_independence(a,b):
                # remove the edge a - b
                self.remove_line(a,b)

    def _find_colliders(self,ci):
        # from d-separation, if a is dependent upon b given c, but is
        # independent without c, then it must be a collider (immorality)

        # reconstruct the complete list of undirected edges in each direction
        lines = self.lines()
        lines += map(swap, lines)
        for a,b in lines:
            # since we are attempting to insert the immorality a -> b <- c
            # better check consistency with the order for a -> b
            if not self.is_potential_parent(a, b):
                continue

            # find every c such that a - b - c (i.e., a -/- c)
            for c in self.neighbours(b) - (frozenset([a]) | self.neighbours(a)):
                # check that c -> b is consistent with any order constraint
                if not self.is_potential_parent(c, b):
                    continue

                # a -> b <- c requires there is no subset S of variables
                # (excluding a and c) that includes b, such that a _||_ c | S.
                if ci.has_independence_involving(a,c,b):
                    continue

                # ensure we aren't about to create a cycle
                if self.directed_path(b,a) or self.directed_path(b,c):
                    raise ICPatternFail,'a cycle would have been created on immorality'+str((a,b,c))
                else:
                    # orient edges as collider
                    # DO NOT remove the original undirected edge!
                    # consider the collider_test below!
                    self.add_arrow(a,b)
                    self.add_arrow(c,b)

        for a,b in self.arrows():
            self.remove_line(a,b)


class InterventionalICPattern(ICPattern):
    def __init__(self, ci=None, constrain_order=None):
        """
        @type ci: None or dict
        @param ci: When dict maps intervention variables to their CI objects.
        frozenset() index indicates observational data.
        """
        if ci is None:
            # copy construct
            super(InterventionalICPattern, self).__init__()
            return

        # use all applicable data to resolve to a Markov equivalence class
        # (or beyond if there are order constraints)
        super(InterventionalICPattern, self).__init__(ci[frozenset()], constrain_order)

        # it is unnecessary to consider the order constraints in the following as
        # all edges that are orientable according to such constraints should already
        # have been applied

        # now do all the interventional monkeying; working relative
        # to the observational data as all relevant data has been included
        # here already (since a factorised model)
        for inter_vars in ci.keys():
            # skip observational data
            if not inter_vars:
                continue

            for b in ci[inter_vars].variables() - inter_vars:
                for a in inter_vars:
                    # if they're already oriented or independent, ignore
                    if a not in self.neighbours(b):
                        continue

                    # in observational model, we have either:
                    # a -> b
                    # or 
                    # a <- b
                    # what's in the interventional data?
                    if ci[inter_vars].has_independence(a, b):
                        # a _|_ b | s in intervened model; so if we
                        # remove all arrows into a they become independent
                        # hence we have a <- b
                        self.remove_line(a, b)
                        self.add_arrow(b, a)
                    else:
                        # otherwise they're still dependent so orient other way
                        self.remove_line(a, b)
                        self.add_arrow(a, b)

        # propagate the implications of any changes
        self.resolve()

# conditional independence tests for use with IC
class CI(object):
    def __init__(self, variables):
        self._variables = frozenset(variables)
        # cache of previously discovered independences
        # a < b: (a,b) -> S where S is a list of separators
        self._ind = {}

    def variables(self):
        return self._variables

    def _index(self,a,b):
        if a > b:
            return (b,a)
        return (a,b)

    def _add_independence(self,a,b,s):
        k = (a,b)
        if a > b:
            k = (b,a)

        if self._ind.has_key(k):
            # keep it all for now
            self._ind[k].add(frozenset(s))
        else:
            self._ind[k] = set([frozenset(s)])

    def has_independence(self, a, b):
        k = (a,b)
        if a > b:
            k = (b,a)
        return self._ind.has_key(k)

    def has_independence_involving(self,a,b,c):
        k = (a,b)
        if a > b:
            k = (b,a)

        if not self._ind.has_key(k):
            return False

        for z in self._ind[k]:
            if c in z:
                return True

        return False

class GraphCI(CI):
    def __init__(self, graph, undirected=False, moralise=False):
        super(GraphCI,self).__init__(variables=graph.vertices())
        self._undir = undirected
        self._graph = graph.copy()
        if undirected and moralise:
            self._graph = self._graph.moralise()

        if undirected:
            self._add_undirected_independencies()
        else:
            self._add_directed_independencies()

        self._verify_independencies()

    def __ior__(self,other):
        for k in other._ind:
            for i in other._ind[k]:
                self._add_independence(k[0],k[1],i)

    def __repr__(self):
        s = ''
        for k,vs in self._ind.iteritems():
            for v in vs:
                s += k[0] + ' _|_ ' + k[1]
                if len(v) != 0:
                    s += ' | ' + v.__repr__()
                s += '\n'
        return s

    def _verify_independencies(self):
        if self._undir:
            for k in self._ind.keys():
                a = frozenset([k[0]])
                b = frozenset([k[1]])
                for cond in self._ind[k]:
                    if not self._graph.separates(a,b, cond):
                        raise RuntimeError,'undirected independence calculation error!'+str((a,b,cond))
        else:
            for k in self._ind.keys():
                a = frozenset([k[0]])
                b = frozenset([k[1]])
                for cond in self._ind[k]:
                    g = self._graph.ancestral_adg(a | b | cond).moralise()
                    if not g.separates(a,b, cond):
                        raise RuntimeError,'directed independence calculation error!'+str((a,b,cond))


    def _add_undirected_independencies(self):
        # global markov property:
        # a _|_ b | s
        # if s separates a and b
        for va, vb in pairs(self._graph.vertices()):
            if self._graph.is_neighbour(va,vb):
                continue
            a = frozenset([va])
            b = frozenset([vb])
            for cond in powerset(self._graph.vertices() - (a|b)):
                cond = set(cond)
                if self.has_independence(va, vb):
                    skip = False
                    for pc in self._ind[self._index(va,vb)]:
                        if pc <= cond:
                            skip = True
                            break
                    if skip:
                        continue
                if self._graph.separates(a,b,cond):
                    self._add_independence(va,vb,cond)

    def _add_directed_independencies(self):
        # directed global markov property:
        # a _|_ b | s
        # where s separates a and b in the moralised graph of
        # the smallest ancestral set of {a,b,s}
        for va,vb in pairs(self._graph.vertices()):
            if self._graph.is_parent(va,vb) or self._graph.is_parent(vb,va):
                continue
            a = frozenset([va])
            b = frozenset([vb])
            for cond in powerset(self._graph.vertices() - (a|b)):
                cond = frozenset(cond)
                if self.has_independence(va, vb):
                    skip = False
                    for pc in self._ind[self._index(va,vb)]:
                        if pc <= cond:
                            skip = True
                            break
                    if skip:
                        continue
                g = self._graph.ancestral_adg(a | b | cond).moralise()
                if g.separates(a,b,cond):
                    self._add_independence(va,vb,cond)

    def independences(self):
        return self._ind

class PCCI(CI):
    """Implementation of Spirtes, Glurmour and Scheines' PC algorithm
    """
    def __init__(self, separator, constrain_order = None, hill_climb_cond = False, must_have=None):
        """
        @param separator: available separator
        @type separator: HypotheticalSeparator
        """
        super(PCCI,self).__init__(variables=separator.variables())
        self._constrain_order = constrain_order
        self._hill_climb_cond = hill_climb_cond
        if must_have is None or not must_have:
            self._must_have = separator.variables()
        else:
            self._must_have = must_have
        self.num_tests = 0
        self._skel = UGraph(vertices=self.variables())
        self._ic_discovery(separator)

    def potential_ancestors(self, child):
        if self._constrain_order is None or child not in self._constrain_order:
            return self.variables()

        i = self._constrain_order.index(child)
        return set(self._constrain_order[:i])

    def skeleton(self):
        return self._skel

    def _ic_discovery(self, separator):
        n = 0
        self._skel.complete(self._skel.vertices())
        skel = self._skel

        too_many_adjacent = True
        while too_many_adjacent:
            too_many_adjacent = False
            # find a pair (x,y) such that the cardinality of the neighbourhood
            # exceeds n
            for x, y in pairs(self.variables()):
                if x not in self._must_have and y not in self._must_have:
                    continue

                n_x = set(skel.neighbours(x))
                n_y = set(skel.neighbours(y))

                if y not in n_x:
                    continue

                if x not in n_y:
                    raise RuntimeError,'inconsistent neighbourhoods'

                # separators must be potential ancestors of both variables.
                # constrain the neighbourhood to contain only potential ancestors
                cond  = n_x & self.potential_ancestors(x)
                cond |= n_y & self.potential_ancestors(y)
                cond -= frozenset([x,y])

                # if the neighbourhood is too small, try the next pair
                if n > len(cond):
                    continue

                # find an untested subset s of neighbours of x of cardinality n
                for s in subsetn(tuple(cond), n):
                    s = frozenset(s)
                    self.num_tests += 1
                    # test for x _|_ y | s
                    if separator.separates(x, y, s):
                        # see if we can find a more probable separator
                        s = self.hill_climb_cond(separator,x,y,s,cond)
                        # record independence since found
                        self._add_independence(x,y,s)
                        skel.remove_line(x, y)
                        break

            # increment required neighbourhood minimum size
            n += 1
            # see if we've found all the CIs
            too_many_adjacent = False
            for x in self.variables():
                too_many_adjacent |= len(skel.neighbours(x)) > n
                if too_many_adjacent:
                    break

    def hill_climb_cond(self, separator, x, y, s, cond):
        """Hill climb over s, trying other elements of
        the set of all possible conditions, cond, to maximise
        the belief.
        
        The idea is that x_|_y|s may just be within the threshold of
        believable, but there exists a slightly large conditional s'
        which we won't find due to the greediness of PC search. finding
        the correct separator influences the collider/immorality recovery
        step of the IC algorithm (the first stage of orienting arrows.)
        Either way, by performing hill climbing search here we should not
        loose out.
        """
        if not self._hill_climb_cond:
            return s

        # start with the current separator s and its degrees of belief
        s = set(s)
        best_score = separator.confidence(x,y,s)
        while True:
            best_new_s = None
            # consider each potential condition
            for new_s in cond - s:
                # test to see if adding just this one condition
                # gives a higher degree of belief
                self.num_tests += 1
                score_new = separator.confidence(x,y,s|set([new_s]))
                if score_new > best_score:
                    # if so, go with it
                    best_score = score_new
                    best_new_s = new_s

            # if we found no improvement, give up (at maximum)
            if best_new_s is None:
                break

            s |= set([best_new_s])
        return frozenset(s)

class HypotheticalSeparator(object):
    def __init__(self, data):
        self._data = data

    def variables(self):
        return self._data.variables()

    def separates(self, x, y, s):
        """Tests if x is separated from y given the separator s. Test rejects
        this hypothesis at p_null.
        @return: True if hypothesis is not rejected
        """
        pass

    def confidence(self, x,y,s):
        """Returns the confidence in the hypothesis x_|_y|s.
        Higher return value should imply a higher confidence."""
        return 0.0

class GraphSeparator(HypotheticalSeparator):
    def __init__(self, graph):
        super(GraphSeparator, self).__init__(None)
        self._graph = graph

    def variables(self):
        return self._graph.vertices()

    def separates(self,x,y,s):
        a = frozenset([x])
        b = frozenset([y])
        g = self._graph.ancestral_adg(a | b | s).moralise()
        return g.separates(a, b, s)

class Chi2Separator(HypotheticalSeparator):
    def __init__(self, data, p_null=0.05):
        """
        @param p_null: threshold at which to reject null hypothesis of conditional independence
        @type p_null: float in (0,1)
        """
        super(Chi2Separator, self).__init__(data)
        self._p_null = p_null

    def separates(self,x,y,s):
        p, x2, d = self.test_independ(x,y,s)
        # fail to reject H_0 of independence?
        return p > self._p_null

    def confidence(self,x,y,s):
        p,x2,d = self.test_independ(x,y,s)
        return p

    def test_independ(self,x,y,s):
        """Test if x _|_ y | s. returns (p,s,d) where p is the probability of
        the available data, given the null hypothesis of independence,
        s is the test statistic, and d is the number of degrees of freedom in
        the available data for this test.
        """
        vars = tuple(s)

        # extract the marginal factors and broadcast out to full instances (for zero censoring)
        joint_counts = self._data.makeFactor(vars + (x,y))
        marginal_counts_x = self._data.makeFactor(vars + (x,))
        marginal_counts_y = self._data.makeFactor(vars + (y,))
        marginal_counts_s = self._data.makeFactor(vars)

        # calculate the number of degrees of freedom. this is equal to
        # (r - 1)(c - 1)\prod_{s \in S} s
        # if there exists a condition s, such that a row or column sum is zero,
        # then we do not count that row or column.
        i_x = sorted(marginal_counts_x.variables()).index(x)
        i_y = sorted(marginal_counts_y.variables()).index(y)
        d = 0
        n_x = self._data.numvals(x)-1
        n_y = self._data.numvals(y)-1
        for s_inst in marginal_counts_s.insts():
            if marginal_counts_s[s_inst] == 0:
                continue

            # count censored x values
            c_x = 0
            x_inst = list(s_inst[:i_x] + (None,) + s_inst[i_x:])
            for v in self._data.values(x):
                x_inst[i_x] = v
                if marginal_counts_x[x_inst] == 0:
                    c_x += 1

            # count censored y values
            c_y = 0
            y_inst = list(s_inst[:i_y] + (None,) + s_inst[i_y:])
            for v in self._data.values(y):
                y_inst[i_y] = v
                if marginal_counts_y[y_inst] == 0:
                    c_y += 1

            # the PC heuristic decrements the cell counts of each row/column by
            # each censored value (provided this does not result in a negative
            # DoF)
            if c_x < n_x and c_y < n_y:
                d += (n_x - c_x)*(n_y - c_y)

        if d <= 0:
            # Chi^2 is undefined!
            # we favour sparse representations
            # H_0 is favouring sparseness
            # this is what Tetrad does.
            return 1.0, 0.0, d

        # convert marginal factor elements to floats from ints
        marginal_counts_x.map(float)
        marginal_counts_y.map(float)
        marginal_counts_s.map(float)
        joint_counts.map(float)

        # expand out all factors to include all relevant variables
        marginal_counts_x = marginal_counts_x.broadcast(joint_counts.variables())
        marginal_counts_y = marginal_counts_y.broadcast(joint_counts.variables())
        marginal_counts_s = marginal_counts_s.broadcast(joint_counts.variables())

        # calculate some statistic that is distributed as Chi^2 though this may
        # only be asymptotically distributed.
        statistic = self.statistic(joint_counts, marginal_counts_x, marginal_counts_y, marginal_counts_s)

        # related to Chi^2 distribution
        # likelihood of independence is P(Chi > X2 | H0)
        # chisqprob is the c.d.f., so we have 1 - P(Chi <= X | H0)
        p = 1 - chisqprob(statistic, d)
        return p, statistic, d


class X2Separator(Chi2Separator):
    def statistic(self, joint_counts, marginal_counts_x, marginal_counts_y, marginal_counts_s):
        """Hypothesis: P(X,Y|Z) = P(X|Z) P(Y|Z)
        Using X^2 = 2 sum_{i \in instances} (x_i - e_i)^2 / e_i
        e_i = x_{i+k}  x_{+jk} /  x_{++k}
        """
        # calculate the expected number of observations assuming the variables
        # are independent
        expected_counts = (marginal_counts_x * marginal_counts_y) / marginal_counts_s

        # calculated the X^2 statistic
        # zero cells disappear (since Factor division defines 0/0 = 0)
        statistic = joint_counts - expected_counts
        statistic.map(square)
        statistic = statistic / expected_counts
        return statistic.z()


class G2Separator(Chi2Separator):
    def statistic(self, joint_counts, marginal_counts_x, marginal_counts_y, marginal_counts_s):
        """Hypothesis: P(X,Y|Z) = P(X|Z) P(Y|Z)
        Using G^2 = 2 sum_{i \in instances} x_i ln (x_i / e_i)
        e_i = x_{i+k}x_{+jk} / x_{++k}
        """
        # calculate the expected number of observations assuming the variables
        # are independent
        expected_counts = (marginal_counts_x * marginal_counts_y) / marginal_counts_s

        # calculate the ln (x_i / e_i) term
        log_exp_joint_counts = joint_counts / expected_counts
        log_exp_joint_counts.map(rlog)

        # now calculate the rest of the G^2 statistic, dropping any entries
        # that turn out to be zero (so imply non-finite elements)
        statistic = joint_counts * log_exp_joint_counts
        return 2*sum([x for x in statistic.data() if is_finite(x)])


class BFSeparator(HypotheticalSeparator):
    def __init__(self, data, belief_threshold=1):
        super(BFSeparator, self).__init__(data)
        self._belief_threshold = log(belief_threshold)

    def separates(self, x, y, s):
        """Hypothesis: P(X,Y|Z) = P(X|Z) P(Y|Z)
        Test using Bayes factor of:
        P(X|Z)P(Y|Z)      P(D|(X_|_Y|Z))
        ------------ = ------------------- = B_f
          P(X,Y|Z)      P(D|not(X_|_Y|Z))
        with P(X,Y|Z) = P(X|Y,Z)P(Y|Z)

        So test P(X|Z)/P(X|Y,Z), and P(Y|Z)/P(Y|X,Z)
        If B_f > p is X_|_Y|Z prefered by K
        p       K
        1--3    barely worth mentioning
        3--10   substantial
        10--30  strong
        this is in keeping with the sparse bias above.
        """
        bf_x, bf_y = self.statistic(x,y,s)

        # there must be a significant degree of belief in both the hypotheses
        # otherwise the inference is invalid.
        if abs(bf_x) <= self._belief_threshold and abs(bf_y) <= self._belief_threshold:
            raise InsufficientBeliefError,'evidence for or against independence was insufficient'

        # do we believe in independence?
        indep_x = bf_x > self._belief_threshold
        indep_y = bf_y > self._belief_threshold

        # there must be agreement between the two tests
        if indep_x != indep_y:
            raise InconsistentBeliefError

        return indep_x

    def confidence(self,x,y,s):
        bf_x, bf_y = self.statistic(x,y,s)

        return bf_x+bf_y

    def statistic(self, x, y, s, data=None):
        vars = tuple(s)
        if data is None:
            data = self._data

        # calculate the marginal and conditional log BDeu scores (marginal log-likelihoods)
        condition_score_x = data.family_score(x, vars + (y,))
        condition_score_y = data.family_score(y, vars + (x,))
        marginal_score_x = data.family_score(x, vars)
        marginal_score_y = data.family_score(y, vars)

        # calculate the Bayes factors for each hypothesis
        bf_x = marginal_score_x - condition_score_x
        bf_y = marginal_score_y - condition_score_y
        return bf_x, bf_y

class CBFSeparator(BFSeparator):
    def __init__(self,data,belief_threshold=1):
        super(CBFSeparator, self).__init__(data.intervention_data(()),belief_threshold)
        self._causal_data = data

    def separates(self,x,y,s):
        vars=set([x,y])
        bf_x, bf_y = 0, 0
        for inter in self._causal_data.interventions():
            if inter & vars:
                continue
            xbf_x, xbf_y = self.statistic(x,y,s,self._causal_data.intervention_data(inter))
            bf_x += xbf_x
            bf_y += xbf_y

        # there must be a significant degree of belief in both the hypotheses
        # otherwise the inference is invalid.
        if abs(bf_x) <= self._belief_threshold and abs(bf_y) <= self._belief_threshold:
            raise InsufficientBeliefError,'evidence for or against independence was insufficient'

        # do we believe in independence?
        indep_x = bf_x > self._belief_threshold
        indep_y = bf_y > self._belief_threshold

        # there must be agreement between the two tests
        if indep_x != indep_y:
            raise InconsistentBeliefError

        return indep_x

class InsufficientBeliefError(Exception):
    pass

class InconsistentBeliefError(Exception):
    pass


class IndepSearch(OrderGraphSearch):
    def __init__(self, hill_climb_cond=True):
        super(IndepSearch, self).__init__()
        self._hill_climb_cond = hill_climb_cond

    def search(self, data, order):
        # no need to separate out interventional data
        ci = PCCI(CBFSeparator(data), constrain_order=order,
                    hill_climb_cond=self._hill_climb_cond)
        skel = ci.skeleton()
        g = ADG(vertices=skel.vertices())
        for a, b in skel.lines():
            if a in ci.potential_ancestors(b):
                g.add_arrow(a, b)
            else:
                g.add_arrow(b, a)
        return g

class FullBFSearch(OrderGraphSearch):
    def __init__(self, hill_climb_cond=True):
        super(FullBFSearch, self).__init__()
        self._hill_climb_cond = hill_climb_cond

    def search(self, data, order):
        ci = {}
        for k in data.interventions():
            if not k:
                ci[k] = PCCI(CBFSeparator(data),
                             constrain_order=order,
                             hill_climb_cond=self._hill_climb_cond)
            else:
                ci[k] = PCCI(BFSeparator(data.intervention_data(k)),
                             constrain_order=order,
                             hill_climb_cond=self._hill_climb_cond)
        if len(ci.keys()) == 1:
            # observational only
            return ICPattern(ci[frozenset()], constrain_order=order)

        # have interventional data too
        return InterventionalICPattern(ci, constrain_order=order)

# try:
#     import psyco
#     psyco.bind(ICPattern)
#     psyco.bind(InterventionalICPattern)
#     psyco.bind(PCCI)
#     psyco.bind(G2Separator)
#     psyco.bind(X2Separator)
#     psyco.bind(BFSeparator)
# except:
#     pass

