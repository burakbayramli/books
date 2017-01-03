"""Random graphs (probability distributions over graphs)

Represent probability distributions over graphs with a fixed set of vertices.
Probability distributions are only represented up to normalisation.
Can also be used to simply represent constraints on graphs so that a uniform distribution
on graphs meeting the constraints is implicitly defined.

@var _version: Version of this module
@type _version: String
"""

_version = '$Id: RandomGraphs.py,v 1.3 2008/10/07 09:12:45 jc Exp $'

from Graphs import ADG, DiGraph

class RandomADG(object):
    """Class for representing probability distributions over
    ADGs (for Bayesian inference of Bayesian networks)

    Adding further vertices after construction is not allowed

    @ivar _lower: A graph containing all arrows that are required.
    @type _lower: L{Graphs.ADG}
    @ivar _upper: A graph containing all arrows that are permitted.
    @type _upper: L{Graphs.DiGraph}
    @ivar _family_scores: If the distribution is conditional on (complete) data, then
    this holds the 'family' components of the BDeu score.
    Generally only a subset of all families will have their score explicitly stored in this way.
    @type _family_scores: Dictionary, one key for each vertex. The value of
    self._family_scores[v] is also a dictionary: each key is a possible parent set for v
    and the corresponding value is that family's contribution to a BDeu score.
    @ivar _pa_lim: Vertices are allowed to have no more parents than this number
    @type _pa_lim: Integer
    @ivar _vertices: The vertices of the random graph
    @type _vertices: Frozenset
    """
    def __init__(self,vertices,lower=None,upper=None,pa_lim=None,vertex_positions=None):
        """Random graph initialisation

        @param vertices: The vertices of the random graph
        @type vertices: Iterable
        @param lower: An acyclic digraph containing all required arrows.
        If set to C{None}, an acyclic digraph on C{vertices} with no arrows is used.
        @type lower: L{Graphs.ADG}
        @param upper: A digraph containing all permitted arrows.
        If set to C{None}, a digraph on C{vertices} contained all possible arrows is used.
        @type upper: L{Graphs.DiGraph}
        @param pa_lim: The maximum number of parents any vertex may have.
        If set to C{None}, C{pa_lim} is |C{vertices}| - 1
        @type pa_lim: Integer
        @param vertex_positions: A mapping from vertices to canvas
        co-ordinates
        @type vertex_positions: Dictionary
        """
        if lower is None:
            lower = ADG(vertices,vertex_positions)
        elif not isinstance(lower,ADG):
            raise ValueError("'lower' (if supplied) has to be an ADG object")
        if upper is None:
            upper = DiGraph(vertices,vertex_positions)
            for vertex in vertices:
                for parent in vertices:
                    if vertex != parent:
                        upper.add_arrow(vertex,parent)
        else:
            if not isinstance(upper,DiGraph):
                raise ValueError("'upper' (if supplied) has to be an DiGraph object")
            if not frozenset(lower.arrows()) <= frozenset(upper.arrows()):
                raise ValueError(
                    "Required arrows not a subset of permitted arrows\nRequired: %s\nPermitted %s\n" %
                    lower.arrows(),upper.arrows())
        if pa_lim is not None:
            for vertex in lower.vertices():
                if lower.num_parents(vertex) > pa_lim:
                    raise ValueError("%s is required to have more parents than the pa_lim argument allows" % vertex)

        self._lower = lower
        self._upper = upper
        self._pa_lim = pa_lim
        self._vertices = frozenset(vertices)

    def adg_score(self,adg):
        """Return the BDeu score of C{adg} assuming 
        BDeu component scores have been computed and stored for it.

        C{condition} is typically used to compute the BDeu scores.
        
        This can return a score for ADGs ruled out hard constraints.

        @param adg: ADG to score
        @type adg: L{ADG}
        @return: The BDeu score of C{adg}.
        @rtype: Float (negative)
        @raise KeyError: If C{adg} contains a vertex-parentset combination for which
        no score has previously been computed.
        """
        return sum(self._family_scores[v][frozenset(adg.parents(v))] for v in adg.vertices())


    def ban_arrow(self,frm,to):
        """Ban an arrow (directed edge) between two
        existing vertices

        It is permitted to ban and arrow which was previously required.

        @param frm: Banned parent
        @type frm: Any immutable type
        @param to: Banned child
        @type to: Any immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._lower.remove_arrow(self,frm,to) # not required
        self._upper.remove_arrow(self,frm,to) # and not permitted


    def ban_arrows(self,non_arrows):
        """Ban a collection of arrows to the graph

        It is permitted to ban and arrow which was previously required.

        @param arrows: The arrows to ban
        @type arrows: An iterator over pairs (of vertices).
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for non_arrow in non_arrows:
            self.ban_arrow(non_arrow[0],non_arrow[1])

    def best_consistent_with_order(self,order):
        """Return the ADG with the highest(*) BDeu score consistent with the
        given ordering, together with its score

        (*) The returned ADG is guaranteed to have the highest score
        if B{all} family scores have been computed and stored (for
        example using the C{condition} method).  Otherwise this method
        just returns the highest scoring ADG from those ADGs all of
        whose families have been computed and stored.

        @param order: Variables in order
        @type order: Iterable
        @return: Best ADG, Score of best ADG
        @rtype: Tuple
        @raise AttributeError: If family scores have not yet been computed
        @raise ValueError: If C{order} is not an ordering of all variables
        """
        if frozenset(order) != frozenset(self._family_scores):
            raise ValueError('%s not an ordering of %s' % (
                order,frozenset(self._family_scores)))
        best = ADG()
        score = 0.0
        order_inv = {}
        for i, v in enumerate(order):
            order_inv[v] = i
        for child, pa_scores in self._family_scores.items():
            child_place = order_inv[child]
            def tmpfun(parents): return -pa_scores[parents]
            for parents in sorted(pa_scores.keys(),key=tmpfun):
                for parent in parents:
                    if order_inv[parent] > child_place:
                        break
                else:
                    # found best parents
                    best.put_family(child,parents)
                    score += pa_scores[parents]
                    #score += int(pa_scores[parents]*100000)
                    #print child, parents, pa_scores[parents]
                    break
        return best, score

    def clauses_justorder(self,predicate='lex_ordered',scaling=1.0,float_weights=False):
        """Directly generate soft clauses for total orderings
        (using lex ordered trick)

        For each variable, and each total order there is a best permissible parent set for that
        variable. Generally, it is not necessary to completely define a total order to determine the best permissible
        parent set. Suppose that parent set {a,b} is the best of all parent sets for variable c, and has score w.
        Then as long as a < c and b < c, then {a,b} is the best choice of parents for c. To express this the clause:
        w : not(a<c) or not(b<c) is generated. Other clauses are also generated so that exactly one is broken given
        any total ordering. The ordering is expressed via a lexicographical ordering, eg a<c is represented as
        lex_ordered(a,c) and c<a as not(lex_ordered(a,c)).

        @param predicate: The name of the predicate used to indicate that two variables are lexicographically ordered.
        @type predicate: String
        @param scaling: How much to multiply scores by to get WCNF weights.
        @type scaling: Float
        @param float_weights: Whether to keep weights as floats. If not they are converted to integers.
        (UBCSAT can deal with floating point weights, so set this to true when using UBCSAT.)
        @type float_weights: Boolean
        @raise IndexError: If a variable has no parentsets stored as candidates
        """

        # will never be called with sorted_parents = []
        def post_clauses(sorted_parents,before,after):

            #assert child not in before | after
            #assert before & after == frozenset()

            contradiction = False
            missing = []
                
            for parent in sorted_parents[0]:
                if parent in after:
                    contradiction = True
                    break
                elif parent not in before:
                    missing.append(parent)

            if contradiction:
                # just move on to the next parents
                for clause in post_clauses(sorted_parents[1:],before,after):
                    yield clause
            else:
                # construct a clause for the best parents: sorted_parents[0]
                score = -pa_scores[sorted_parents[0]]*scaling
                if float_weights:
                    clause = [score]
                else:
                    clause = [int(score)]
                for parent in before | frozenset(missing):
                    if parent < child:
                        clause.append( (False,predicate,parent,child) )
                    else:
                        clause.append( (True,predicate,child,parent) )
                for variable in after:
                    if variable < child:
                        clause.append( (True,predicate,variable,child) )
                    else:
                        clause.append( (False,predicate,child,variable) )
                yield tuple(clause)

                # fire off mutually exclusive orders
                for i, parent in enumerate(missing):
                    for clause in post_clauses(sorted_parents[1:],
                                               before | frozenset(missing[i+1:]),
                                               after  | frozenset([parent])):
                        yield clause


        for child, pa_scores in self._family_scores.items():
            def tmpfun(parents): return -pa_scores[parents]
            sorted_parents = tuple(sorted(pa_scores.keys(),key=tmpfun))
            for clause in post_clauses(sorted_parents,frozenset(),frozenset()):
                yield clause


    def clauses_antisymmetry(self,predicate='ancestor'):
        """Generate hard clauses stating that
        forall v,w in vertices where v!=w :  not predicate(v,w) \/ not predicate(w,v)

        @param predicate: The name of the anti-symmetric predicate
        @type predicate: String
        """
        for v in self._vertices:
            for w in self._vertices:
                if v != w:
                    yield ('hard', (False,predicate,v,w), (True,predicate,w,v) )


    def clauses_extension(self,antecedent='parent',consequent='ancestor',vertices_guard=None):
        """Generate clauses  for all v,w in vertices where vertices_guard(v,w) stating that
        antecedent(v,w) -> consequent(v,w)

        if vertices_guard is None it is set to v!=w, so all distinct ordered pairs considered.
        An alternative could be self.foo where def foo(self,v,w): return w in self._upper.is_parent(v,w)

        @param antecedent: Name of antecedent predicate
        @type antecdent: String
        @param consequent: Name of consequent predicate
        @type antecdent: String
        @param vertices_guard: Name of 'guard' predicate, if any.
        @type antecdent: String or None
        """
        if vertices_guard is None:
            def vertices_guard(v,w): return v != w
        for v in self._vertices:
            for w in self._vertices:
                if vertices_guard(v,w):
                    yield ('hard', (False,antecedent,v,w), (True,consequent,v,w) )

    def clauses_parent_choice(self,predicate='has_parents',scaling=1.0,float_weights=False):
        """Generate soft clauses for family choices

        For each variable, both soft clauses encoding a score for each parent choice
        and a single hard clause asserting that one of the parent choices is chosen are generated

        Since each score is rounded to an integer, scaling > 1 can be used to reduce
        a loss of precision

        @parame predicate: Name of binary predicate asserting that a child has a certain set of parents
        @param predicate: String
        @param scaling: How much to multiply scores by to get WCNF weights.
        @type scaling: Float
        @param float_weights: Whether to keep weights as floats. If not they are converted to integers.
        (UBCSAT can deal with floating point weights, so set this to true when using UBCSAT.)
        @type float_weights: Boolean
        """
        if float_weights:
            def weight(x): return -x*scaling
        else:
            def weight(x): return -int(x*scaling)

        for v in self._vertices:
            for pas, w in self._family_scores[v].items():
                yield (weight(w), (False,predicate,v,pas))
            yield tuple(['hard'] + [(True,predicate,v,parents) for parents in pas])

    def clauses_parent_choice_consequence(self,antecedent='has_parents',consequent='ancestor'):
        """Generate clauses stating that choosing a particular parent set for a variable
        implies a number of C{consequent} relations on vertices.

        eg if v is chosen to have parents w and z then both w and z are ancestors of v

        @param antecedent: Name of binary predicate asserting that a child has a certain set of parents
        @param antecedent: String
        @param consequent: Name of binary predicate asserting the relation implied by C{antecedent}
        @type consequent: String
        """
        for child, dkt in self._family_scores.items():
            for pas in dkt:
                antecedent_literal = (False,antecedent,child,pas)
                for parent in pas:
                    yield ('hard', antecedent_literal, (True,consequent,parent,child))
                    

    def clauses_parent_choice_consequence_lex_order(self,antecedent='has_parents',consequent='lex_ordered'):
        """Generate clauses stating that choosing a particular parent set for a variable
        implies a number of C{consequent} relations on vertices, the truth values of which are a function
        of whether the two variables are lexicographically ordered.

        eg if v is chosen to have parents w and z then both w and z must come before v in any associated
        total ordering. See source for further details.

        @param antecedent: Name of binary predicate asserting that a child has a certain set of parents
        @param antecedent: String
        @param consequent: Name of binary predicate asserting the relation implied by C{antecedent}
        @type consequent: String
        """
        for child, dkt in self._family_scores.items():
            for pas in dkt:
                antecedent_literal = (False,antecedent,child,pas)
                for parent in pas:
                    if parent < child:
                        yield ('hard', antecedent_literal, (True,consequent,parent,child))
                    else:
                        yield ('hard', antecedent_literal, (False,consequent,child,parent))

    def clauses_transitivity(self,predicate):
        """Generate clauses stating that a binary predicate on vertices is transitive

        @param predicate: The name of the transitive predicate
        @type predicate: String
        """
        for v in self._vertices:
            for w in self._vertices:
                if v == w:
                    continue
                for z in self._vertices:
                    if z == v or z== w:
                        continue
                    yield ('hard', (False,predicate,v,w),
                           (False,predicate,w,z), (True,predicate,v,z))

    def clauses_no3cycles(self,predicate='lex_ordered'):
        """Generate clauses suitable for ruling out 3 cycles in a tournament

        note that only literals of the form (sign,predicate,v,w) where v<w are
        created

        @param predicate: The name of the binary predicate
        @type predicate: String
        """
        variables = sorted(self._vertices)
        for i, third in enumerate(variables):
            for j, second in enumerate(variables[:i]):
                for first in variables[:j]:
                    yield ('hard', (False,predicate,first,second),
                           (False,predicate,second,third), (True,predicate,first,third))
                    yield ('hard', (True,predicate,first,second),
                           (True,predicate,second,third), (False,predicate,first,third))
                    

    def condition(self,data,filter=True,precision=1.0):
        """Condition on data

        More exactly, BDeu component scores are computed and stored for all
        legal (within parent limit threshold) families.

        Any previous family scores will be deleted.
        Only scores for 'legal' families will be computed.

        The scores are computed 'naively'. Each marginal contingency table is
        computed by a fresh pass over the data, then all BDeu score computable from
        this marginal contingency table are made.

        @param data: Complete discrete data
        @type data: Any class providing a method called C{makeFactor} which takes an
        iterable of variable names as its sole argument and which returns the corresponding
        C{Factor} object.
        @param filter: If true, a parent set is only stored if its score is higher than
        that of all its subsets.
        @type filter: Boolean
        @param precision: The precision for the BDeu score
        @type precision: Float
        """
        from gPy.Utils import subseteqn
        from gPy.Parameters import CPT
        family_scores = dict([(v,{}) for v in self._lower.vertices()])
        for size in range(1,self._pa_lim+1):
            for family in subseteqn(self._variables,size):
                factor = data.makeFactor(family)
                for i, ch in enumerate(family):
                    parents = frozenset(family[:i]+family[i+1:])

                    if not (self._upper.is_subset_of_parents(ch,parents)
                        and self._lower.is_superset_of_parents(ch,parents)):
                        continue
                    
                    score = CPT(factor,ch).bdeu_score(precision)

                    family_scores_ch = family_scores[ch]
                    if filter:
                        for paset, paset_score in family_scores_ch.items():
                            if paset <= parents and paset_score >= score:
                                break
                        else:
                            family_scores_ch[parents] = score
                    else:
                        family_scores_ch[parents] = score
        self._family_scores = family_scores


    def condition_from_dict(self,dkt):
        """Condition on data using pre-computed scores

        More exactly, BDeu component scores are stored for all
        legal families.

        A copy of C{dkt} is used.
        
        Any previous family scores will be deleted.
        Only scores for 'legal' families will be computed.

        @param dkt: Dictionary of family scores. dkt[v][frozenset([a,b,c])] has
        the score for v having parents a, b and c.
        @type dkt: Dictionary
        """
        dkt = dkt.copy()
        for ch, parentsets in dkt.items():
            for parents in parentsets.keys():
                if not (self._upper.is_subset_of_parents(ch,parents)
                        and self._lower.is_superset_of_parents(ch,parents)):
                    del dkt[ch][parents]
        self._family_scores = dkt


    def family_scores(self):
        """Return (a copy of) the dictionary containing BDeu component scores

        @return: A copy of the dictionary containing BDeu component scores
        @rtype: Dictionary
        """
        import copy
        return copy.deepcopy(self._family_scores)


    def feasible(self,adg):
        """Whether C{adg} is one of the ADGs permitted by C{self}

        It is assumed that C{adg} has the same vertices as C{self}

        @param adg: A candidate acyclic digraph
        @type adg: L{Graphs.ADG}
        @return: Whether C{adg} is permitted by C{self}
        @rtype: Boolean
        @raise KeyError: If C{adg} contains a vertex not in C{self}.
        """
        for child in adg.vertices():
            parents = adg.parents(child)
            if len(parents) > self._pa_lim:
                return False
            if not (self._upper.is_subset_of_parents(child,parents)
                    and self._lower.is_superset_of_parents(child,parents)):
                return False
        return True

    def non_arrows(self):
        """Return a list of banned arrows

        @return: A list of banned arrows
        @rtype: List of C{(vertex,child)} tuples
        """
        non_arrows = []
        for vertex in self._vertices:
            for child in self._vertices:
                if vertex != child and not self._upper.is_parent(vertex,child):
                    non_arrows.append((vertex,child))
        return non_arrows

    def require_arrow(self,frm,to):
        """Require an arrow (directed edge) between two
        vertices

        @param frm: Parent
        @type frm: Any immutable type
        @param to: Child
        @type to: Any immutable type
        @raise KeyError: If either vertex does not exist
        @raise DirectedCycleError: If requiring this arrows means that
        all contain a cycle.
        @raise ValueError: If requiring this arrow breaks the upper bound on
        the number of parents a vertex may have
        """
        if self._pa_lim is not None and lower.num_parents(to) == self._pa_lim:
            raise ValueError('%s already has the maximum number of parents' % to)
        self._lower.add_arrow(self,frm,to) # required
        self._upper.add_arrow(self,frm,to) # and permitted

    def require_arrows(self,arrows):
        """Require a collection of arrows to the graph

        @param arrows: The arrows to require
        @type arrows: An iterator over pairs (of vertices).
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for arrow in arrows:
            self.require_arrow(arrow[0],arrow[1])

    def rescore(self,fobj,outfile,inv_atom_ids,predicate='has_parents'):
        """Compute and output posterior probabilities (uniform prior) for a collection of ADGs
        stored (and encoded in a file) 

        @param fobj: File containing ADGs encoded using integers
        @type fobj: Readable file object (B{not} the filename)
        @param inv_atom_ids: Maps integers to ground logical atoms. This will
        have typically been produced by L{write_cnf}.
        @param outfile: Output file
        @type inv_atom_ids: List
        @param predicate: The ground logical atoms recovered using C{inv_atom_ids} are assumed to be
        tuples of the form C(predicate,child,pas)} where C{child} is a vertex and C{pas} is a frozenset
        giving its parents.
        @type predicate: String
        """

        from heapq import heappush, heappop
        from math import exp
        heap = []
        for line in fobj:
            outline = ''
            logprob = 0
            for family in line.rstrip().split()[2:]:
                atom = inv_atom_ids[int(family)]
                if atom[0] == predicate:
                    ch, pas = atom[1], atom[2]
                    logprob += self._family_scores[ch][pas]
                    outline += '%s-%s;' % (ch,sorted(pas))
                    heappush(heap,(-logprob,outline))

        # best, most negative is first
        # best = heap[0][0]
        z = sum(exp(-item[0]) for item in heap)
        print >>outfile, z
        while heap:
            neglogprob, outline = heappop(heap)
            print >>outfile, exp(-neglogprob)/z, outline
            

    def unban_arrow(self,frm,to):
        """Permit an arrow (directed edge) between two
        vertices

        @param frm: Parent
        @type frm: Any immutable type
        @param to: Child
        @type to: Any immutable type
        @raise KeyError: If either vertex does not exist
        @raise DirectedCycleError: If this arrow, together with
        """
        self._upper.add_arrow(self,frm,to)


    def unban_arrows(self,arrows):
        """Permit a collection of arrows to the graph

        @param arrows: The arrows to permit
        @type arrows: An iterator over pairs (of vertices).
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for arrow in arrows:
            self.unban_arrow(arrow[0],arrow[1])

    def unrequire_arrow(self,frm,to):
        """Remove any requirement for an arrow (directed edge) between two
        vertices

        @param frm: Parent
        @type frm: Any immutable type
        @param to: Child
        @type to: Any immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._lower.remove_arrow(self,frm,to) # required


    def unrequire_arrows(self,arrows):
        """Remove any requirement for an arrow (directed edge) between two
        vertices for a collection of arrows

        @param arrows: The arrows which are not required
        @type arrows: An iterator over pairs (of vertices).
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for arrow in arrows:
            self.unrequire_arrow(arrow[0],arrow[1])

    def upper(self):
        """Return a digraph containing all permitted arrows
        
        @return: A digraph containing all permitted arrows
        @rtype: L{Graphs.DiGraph}
        """
        return self._upper.copy()


    def vertices(self):
        """Return the vertices of the random graph

        @return: The vertices of the random graph
        @rtype: Frozenset
        """
        return self._vertices

    def yield_sample(self,fobj,inv_atom_ids,predicate='has_parents'):
        """Spit out the ADGs encoded in a file

        ADGs are generated as lists of (child,parent)

        @param fobj: File containing ADGs encoded using integers
        @type fobj: Readable file object (B{not} the filename)
        @param inv_atom_ids: Maps integers to ground logical atoms. This will
        have typically been produced by L{write_cnf}.
        @param outfile: Output file
        @type inv_atom_ids: List
        @param predicate: The ground logical atoms recovered using C{inv_atom_ids} are assumed to be
        tuples of the form C(predicate,child,pas)} where C{child} is a vertex and C{pas} is a frozenset
        giving its parents.
        @type predicate: String
        """
        for line in fobj:
            item = []
            logprob = 0
            for family in line.rstrip().split()[2:]:
                 atom = inv_atom_ids[int(family)]
                 if atom[0] == predicate:
                     ch, pas = atom[1], atom[2]
                     logprob += self._family_scores[ch][pas]
                     item.append((ch,pas))
            yield item, logprob

class EstRandomADG(object):
    """Class for representing approximations to probability
    distributions over ADGs of a common set of variables (for Bayesian
    inference of Bayesian networks)

    An instance is essentially a sample (i.e. a set) of ADGs, each
    labelled with its probability up to a multiplicative normalising
    factor. The sum of these probabilities is also stored. By
    approximating the probability of each ADG not in the sample by 0, an estimate
    of the probability of all those present in sample can be easily found.

    Each ADG is representing by a tuple of parent sets, one parent set
    for each variable. The parent sets in the tuple are ordered
    according to the lexicographic order of the variables.

    Store first ADG and then subsequent ADGs are indicated by which family changes.

    @ivar _first: First ADG
    @type _first: Tuple
    @ivar _probsum: Sum of (unnormalised) probabilities of each ADG in the sample
    @type _probsum: Float
    @ivar _sample: Tuple, each element of which is (edit,prob). Edit is also a tuple indicating
    how to edit the previous ADG to get this one. prob is the unnormalised probability
    @type _sample: Tuple
    """
    def __init__(self,sample):
        """C{sample} is an iterator returning (logprob,(ch1,pas1),(ch2,pas2),..)) tuples.
        """
        
        from math import exp

        first, logprob = sample.next()
        z = exp(logprob)
        
        current = {}
        for ch, pas in first:
            current[ch] = pas
        self._first = current.copy()

        self._sample = [(z,{})]
        for adg, logprob in sample:
            new = {}
            for ch, pas in adg:
                if pas != current[ch]:
                    new[ch] = pas
            unnorm_prob = exp(logprob)
            self._sample.append((unnorm_prob,new))
            z += unnorm_prob
            current.update(new)

        self._probsum = z
                    
    def exp(self,f):
        """ C{f} is a function taking a dictionary mapping children to frozenset parents
        as input and returning a number
        """
        current = self._first.copy()
        expvalue = 0
        for prob, dkt in self._sample:
            current.update(dkt)
            expvalue += (prob * f(current))
        return expvalue / self._probsum

#     originally a method for RandomGraph
#     def condition_sql(self,data,filter=True):
#         """Condition on data

#         @param data: Complete discrete data
#         @type data: L{Parameters.SqliteFactor}
#         @param filter: If true, a parent set is only stored if its score is higher than
#         that of all its subsets.
#         @type filter: Boolean
#         """
#         from gPy.Utils import subseteqn
#         family_scores = self._family_scores
#         previous_layer = {frozenset():data.score([])}
#         sortedvars = tuple(sorted(self._variables))
#         for size in range(1,self._pa_lim+1):
#             layer = {}
#             scores, indices_s = data.score_all(size)
#             for i, fam_score in enumerate(scores):
#                 family = frozenset([sortedvars[j] for j in indices_s[i]])
#                 layer[family] = fam_score
#                 for ch in family:
#                     family_scores_ch = family_scores[ch]
#                     parents = family.difference([ch])
#                     this_score = fam_score - previous_layer[parents]
#                     if filter:
#                         for paset, paset_score in family_scores_ch.items():
#                             if paset <= parents and paset_score >= this_score:
#                                 # print '\t\tFiltered since', paset, paset_score
#                                 break
#                         else:
#                             family_scores_ch[parents] = this_score
#                             #print '\t\tStored'
#                     else:
#                         family_scores_ch[parents] = this_score
#             previous_layer = layer


#     def write_cnf2(self,filename,encode_arrows=True,
#                   scaling=1,ancestors=frozenset(),nonancestors=frozenset(),indeps=frozenset(),comments=True):
#         """Like write_cnf, but don't bother with acyclicity atom
#         """

#         from os.path import abspath
        
#         # Encode 'A->B' and 'A->>B' as integers for all A and B
#         atom = 1
#         pa_atom = {}
#         an_atom = {}
#         fobj = open(filename,'w')
#         family_atoms = {}
#         self._wcnf_info['family_atoms'] = family_atoms
#         self._wcnf_info['filename'] = abspath(filename)
#         self._wcnf_info['encode_arrows'] = encode_arrows
#         self._wcnf_info['scaling'] = scaling
#         self._wcnf_info['pa_atom'] = pa_atom
#         self._wcnf_info['an_atom'] = an_atom
#         variables = self._variables
#         n = len(variables)
#         if encode_arrows:
#             offset = n*(n-1)
#         else:
#             offset = 0
#         for v in variables:
#             for w in variables:
#                 if v == w:
#                     continue
#                 pair = v,w
#                 if encode_arrows:
#                     pa_atom[pair] = atom
#                     if comments:
#                         print >>fobj, 'c Atom %d means %s -> %s' % (atom,v,w)
#                 an_atom[pair] = atom+offset
#                 if comments:
#                     print >>fobj, 'c Atom %d means %s ->> %s' % (atom+offset,v,w)
#                 atom += 1
#         atom += offset

#         hard_clauses = []
#         soft_clauses = []

#         if encode_arrows:
#             for (v,w), pa_vw in pa_atom.items():
#                 # parents are ancestors
#                 hard_clauses.append((-pa_vw,an_atom[v,w]))
#                 if comments:
#                     print >>fobj, 'c Clause %d %d: %s -> %s => %s ->> %s' % (-pa_vw,an_atom[v,w],v,w,v,w) 

#         no_isolates = {}
#         for v in variables:
#             no_isolates[v] = []

#         for (v,w), an_vw in an_atom.items():

#             # all variables connected
#             no_isolates[v].append(an_vw)
#             no_isolates[w].append(an_vw)

#             if (v,w) in ancestors:
#                 hard_clauses.append((an_vw,))
#                 if comments:
#                     print >>fobj, ('c Clause %d : %s  an ancestor of %s'
#                                    % (an_vw, v,w) )
#             if (v,w) in nonancestors:
#                 hard_clauses.append((-an_vw,))
#                 if comments:
#                     print >>fobj, ('c Clause %d : %s  an non ancestor of %s'
#                                    % (-an_vw, v,w) )

            
#             # if v ancestor of w, and w ancestor of v then a cycle
#             if frozenset((v,w)) in indeps:
#                 hard_clauses.append((-an_vw,))
#                 if comments:
#                     print >>fobj, ('c Clause %d : %s not an ancestor of %s'
#                                    % (-an_vw, v,w) )

#             else:
#                 hard_clauses.append((-an_vw,-an_atom[w,v]))
#                 if comments:
#                     print >>fobj, ('c Clause %d %d: %s ->> %s & %s ->> %s not allowed'
#                                    % (-an_vw,-an_atom[v,w],v,w,w,v) )
#             for z in variables:
#                 if (v == z) or (z == w):
#                     continue
#                 # if z ancestor of v and v ancestor of w, then z ancestor of w
#                 if frozenset((v,w)) in indeps:
#                     hard_clauses.append((-an_atom[z,v],-an_atom[z,w]))
#                     if comments:
#                         print >>fobj, ('c Clause %d %d : %s not an ancestor of %s or %s not an acestor of %s'
#                                        % (-an_atom[z,v],-an_atom[z,w],z,v,z,w))

#                 else:
#                     hard_clauses.append((-an_atom[z,v],-an_vw,an_atom[z,w]))
#                     if comments:
#                         print >>fobj, ('c Clause %d %d %d: %s ->> %s & %s ->> %s => %s ->> %s' %
#                                        (-an_atom[z,v],-an_vw,an_atom[z,w],z,v,v,w,z,w)) 

#         for stuff in no_isolates.values():
#             hard_clauses.append(tuple(stuff))
#             if comments:
#                 print >>fobj, 'c Clause for no isolates ', stuff

#         # create atoms stating the existence of families
#         # and create weighted clauses to encode their scores
#         self._wcnf_info['printatoms'] = []
#         for ch, pa_scores in self._family_scores.items():
#             first_ch_lit = atom
#             for parent_set, llh_score in pa_scores.items():
#                 ok = True
#                 for parent in parent_set:
#                     if frozenset((ch,parent)) in indeps:
#                         ok = False
#                         break
#                 if not ok:
#                     continue
#                 if comments:
#                     print >>fobj, ('c Atom %d means that %s has parents %s' %
#                                    (atom,ch,parent_set))
#                 family_atoms[atom] = (ch,parent_set)
#                 for parent in parent_set:
#                     if encode_arrows:
#                         if comments:
#                             print >>fobj, ('c Clause %d %d: %s has parents %s => %s -> %s' %
#                                            (-atom,pa_atom[parent,ch],ch,parent_set,parent,ch))
#                         hard_clauses.append((-atom,pa_atom[parent,ch]))
#                     else:
#                         if comments:
#                             print >>fobj, ('c Clause %d %d: %s has parents %s => %s ->> %s' %
#                                            (-atom,an_atom[parent,ch],ch,parent_set,parent,ch))
#                         hard_clauses.append((-atom,an_atom[parent,ch]))
                
#                     # currently don't add constraint about implied non-parents

#                     # soft constraint from data
#                 if comments:    
#                     print >>fobj, ('c Soft clause %d %d: since %s with parents %s has score %f and scaling is %d' %
#                                    (int(-llh_score*scaling),-atom,ch,parent_set,llh_score,scaling))
#                 soft_clauses.append((int(-llh_score*scaling),-atom))
#                 atom += 1
#             family_lits = range(first_ch_lit,atom)
#             if comments:
#                 print >>fobj, 'c Clause %s: since exactly one family for %s' % (
#                     ' '.join([str(x) for x in family_lits]), ch)
#             hard_clauses.append(tuple(family_lits))

#             self._wcnf_info['printatoms'].extend(family_lits)

#         # put in constraints so that only 1 representative of a Markov equivalence class exists
#         #for atom, (ch,parent_set) in family_atoms.items():
#         #    for atom2, (parent,parent_set2) in family_atoms.items():
#         #        if (parent >= ch) and (parent in parent_set) and (parent_set == parent_set2 | set([parent])):
#         #            hard_clauses.append((-atom,-atom2))
#         #            if comments:
#         #                 print >>fobj, 'c Disallow %s having parents %s when %s has parents %s' % (
#         #                     ch, parent_set, parent, parent_set2)
                        
#         # print out header
#         print >>fobj, 'p wcnf %d %d' % (atom-1,len(hard_clauses)+len(soft_clauses))

#         # print out soft clauses

#         maxcost = 0
#         for clause in soft_clauses:
#             maxcost = max(maxcost,clause[0])
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'

#         hard_clause_cost = maxcost * len(self._family_scores)
#         if hard_clause_cost > 2147483647:
#             raise ValueError('%d too big' % hard_clause_cost)

#         # print out hard clauses
#         for clause in hard_clauses:
#             print >>fobj, '%d ' % hard_clause_cost,
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'
#         fobj.close()

#         self._wcnf_info['next_atom'] = atom
        
#         return atom


#     def write_cnf3(self,filename,
#                   scaling=1,comments=True):
#         """Encode parent sets as combinations of literals
#         """

#         from os.path import abspath
        
#         current_atom = 1
#         pa_atom = {}
#         an_atom = {}
#         fobj = open(filename,'w')
#         self._wcnf_info['filename'] = abspath(filename)
#         self._wcnf_info['scaling'] = scaling
#         self._wcnf_info['pa_atom'] = pa_atom
#         self._wcnf_info['an_atom'] = an_atom

#         # set up atoms

#         tmp_an = {}
#         dg = self.make_domain_graph()
#         for child in dg.vertices():
#             for parent in dg.parents(child):
#                 pa_atom[parent,child] = current_atom
#                 if comments:
#                     print >>fobj, 'c Atom %d means %s -> %s' % (current_atom,parent,child)
#                 current_atom += 1
#         for child in dg.vertices():
#             tmp_an[child] = set()
#             for ancestor in dg.ancestors(child):
#                 if ancestor != child:
#                     an_atom[ancestor,child] = current_atom
#                     tmp_an[child].add(ancestor)
#                     if comments:
#                         print >>fobj, 'c Atom %d means %s ->> %s' % (current_atom,ancestor,child)
#                     current_atom += 1

#         hard_clauses = []

#         # set up transitivity clauses

#         for (parent,child), atom in pa_atom.items():
#             hard_clauses.append((-atom,an_atom[parent,child]))
#             if comments:
#                     print >>fobj, 'c Clause %d %d means %s -> %s => %s ->> %s' % (
#                         -atom,an_atom[parent,child],parent,child,parent,child)
#             for ancestor in tmp_an[parent]:
#                 if ancestor != child:
#                     hard_clauses.append((-atom,-an_atom[ancestor,parent],an_atom[ancestor,child]))
#                     if comments:
#                         print >>fobj, 'c Clause %d %d %d means %s -> %s & %s ->> %s => %s ->> %s' % (
#                         -atom,-an_atom[ancestor,parent],an_atom[ancestor,child],
#                         parent,child,ancestor,parent,ancestor,child)

#         # set up acyclicity constraints
        
#         for (parent,child), atom in pa_atom.items():
#             try:
#                 hard_clauses.append((-atom,-an_atom[child,parent]))
#                 if comments:
#                     print >>fobj, 'c Clause %d %d means %s -> %s => not(%s ->> %s)' % (
#                         -atom,-an_atom[child,parent],parent,child,child,parent)
#             except KeyError:
#                 if comments:
#                     print >> fobj, 'c Impossible for %s -> %s to cause a cycle so no constraint' % (
#                         parent,child)

#         # set up constraints that only parent sets specified in _family_scores are permissible choices.


#         soft_clauses = []

#         # create soft clauses encoding family scores
        
#         for child, pa_scores in self._family_scores.items():
#             tmp = []
#             for parent_set, llh_score in pa_scores.items():
#                 clause = [-pa_atom[parent,child] for parent in parent_set]
#                 also = set()
#                 for other in pa_scores:
#                     if other >= parent_set:
#                         also.update(other - parent_set)
#                 for other_parent in also:
#                     clause.append(pa_atom[other_parent,child])
#                 soft_clauses.append(tuple([int(-llh_score*scaling)]+clause))
#                 tmp.append(clause)
#                 if comments:
#                     print >>fobj, 'c Soft clause %d %s: since %s with parents %s has score %f and scaling is %d' % (
#                         int(-llh_score*scaling),clause,child,parent_set,llh_score,scaling)
#             for neg_clause in neg_cnf(tmp):
#                 hard_clauses.append(tuple(neg_clause))
#                 if comments:
#                     print >> fobj, 'c %s part of CNF ruling out illegal parent choices for %s' % (
#                         tuple(neg_clause),child)


#         # print out header
#         print >>fobj, 'p wcnf %d %d' % (current_atom-1,len(hard_clauses)+len(soft_clauses))

#         # print out soft clauses

#         maxcost = 0
#         for clause in soft_clauses:
#             maxcost = max(maxcost,clause[0])
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'

#         hard_clause_cost = maxcost * len(self._family_scores)
#         if hard_clause_cost > 2147483647:
#             raise ValueError('%d too big' % hard_clause_cost)

#         # print out hard clauses
#         for clause in hard_clauses:
#             print >>fobj, '%d ' % hard_clause_cost,
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'
#         fobj.close()

#         self._wcnf_info['next_atom'] = current_atom
        
#         return current_atom


#     def write_cnf4(self,filename,
#                   scaling=1,comments=True,lim=2):
#         """Encode parent sets as combinations of literals

#         Only rule out cycles of length 2
#         """

#         from os.path import abspath
        
#         current_atom = 1
#         pa_atom = {}
#         fobj = open(filename,'w')
#         self._wcnf_info['filename'] = abspath(filename)
#         self._wcnf_info['scaling'] = scaling
#         self._wcnf_info['pa_atom'] = pa_atom

#         # set up atoms

#         dg = self.make_domain_graph()
#         for child in dg.vertices():
#             for parent in dg.parents(child):
#                 pa_atom[parent,child] = current_atom
#                 if comments:
#                     print >>fobj, 'c Atom %d means %s -> %s' % (current_atom,parent,child)
#                 current_atom += 1

#         hard_clauses = []

#         # set up acyclicity constraints

#         for cycle in dg.enumerate_cycles(lim):
#             clause = []
#             for i in range(len(cycle)-1):
#                 clause.append(-pa_atom[cycle[i],cycle[i+1]])
#             hard_clauses.append(tuple(clause))
#             if comments:
#                 print >> fobj, 'c Clause %s rules out the cycle %s' % (clause,cycle)
                
#         soft_clauses = []

#         # create soft clauses encoding family scores
        
#         for child, pa_scores in self._family_scores.items():
#             tmp = []
#             for parent_set, llh_score in pa_scores.items():
#                 clause = [-pa_atom[parent,child] for parent in parent_set]
#                 also = set()
#                 for other in pa_scores:
#                     if other >= parent_set:
#                         also.update(other - parent_set)
#                 for other_parent in also:
#                     clause.append(pa_atom[other_parent,child])
#                 soft_clauses.append(tuple([int(-llh_score*scaling)]+clause))
#                 tmp.append(clause)
#                 if comments:
#                     print >>fobj, 'c Soft clause %d %s: since %s with parents %s has score %f and scaling is %d' % (
#                         int(-llh_score*scaling),clause,child,parent_set,llh_score,scaling)
#             for neg_clause in neg_cnf(tmp):
#                 hard_clauses.append(tuple(neg_clause))
#                 if comments:
#                     print >> fobj, 'c %s part of CNF ruling out illegal parent choices for %s' % (
#                         tuple(neg_clause),child)

#         # print out header
#         print >>fobj, 'p wcnf %d %d' % (current_atom-1,len(hard_clauses)+len(soft_clauses))

#         # print out soft clauses

#         maxcost = 0
#         for clause in soft_clauses:
#             maxcost = max(maxcost,clause[0])
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'

#         hard_clause_cost = maxcost * len(self._family_scores)
#         if hard_clause_cost > 2147483647:
#             raise ValueError('%d too big' % hard_clause_cost)

#         # print out hard clauses
#         for clause in hard_clauses:
#             print >>fobj, '%d ' % hard_clause_cost,
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'
#         fobj.close()

#         self._wcnf_info['next_atom'] = current_atom
        
#         return current_atom

#     def write_cnf5(self,filename,scaling=1,cycle_lim=2,comments=True):
#         """Just use family atoms
#         """

#         from os.path import abspath
        
#         current_atom = 1
#         fobj = open(filename,'w')
#         family_atoms = {}
#         self._wcnf_info['family_atoms'] = family_atoms
#         self._wcnf_info['filename'] = abspath(filename)
#         self._wcnf_info['scaling'] = scaling

#         hard_clauses = []
#         soft_clauses = []

#         # create atoms stating the existence of families
#         # and create weighted clauses to encode their scores
#         inv = {}
#         for ch, pa_scores in self._family_scores.items():
#             first_ch_lit = current_atom
#             for parent_set, llh_score in pa_scores.items():
#                 print >>fobj, 'c Atom %d means that %s has parents %s' % (
#                     current_atom,ch,parent_set)
#                 family_atoms[current_atom] = (ch,parent_set)
#                 inv[ch,parent_set] = current_atom
#                 # soft constraint from data
#                 if comments:    
#                     print >>fobj, 'c Soft clause %d %d: since %s with parents %s has score %f and scaling is %d' % (
#                         int(-llh_score*scaling),-current_atom,ch,parent_set,llh_score,scaling)
#                 soft_clauses.append((int(-llh_score*scaling),-current_atom))
#                 current_atom += 1
#             family_lits = range(first_ch_lit,current_atom)
#             if comments:
#                 print >>fobj, 'c Clause %s: since exactly one family for %s' % (
#                     ' '.join([str(x) for x in family_lits]), ch)
#             hard_clauses.append(tuple(family_lits))

#         dg = self.make_domain_graph()
#         kk = 0
#         for cycle in dg.enumerate_cycles(cycle_lim):
#             #print cycle
#             kk += 1
#         print kk

#         kk = 0
#         for i, v1 in enumerate(self._variables):
#             #print v1
#             for parents1 in self._family_scores[v1]:
#                 if not parents1:
#                     continue
#                 #print '\t', parents1
#                 for v2 in self._variables[i+1:]:
#                     #print '\t\t', v2, kk
#                     for parents2 in self._family_scores[v2]:
#                         if not parents2:
#                             continue
#                         # look for 2-cycles
#                         if v2 in parents1 and v1 in parents2:
#                             clause = (-inv[v1,parents1],-inv[v2,parents2])
#                             hard_clauses.append(clause)
#                             kk += 1
#                             if comments:
#                                 print >> fobj, 'c Clause %s stops cycle: %s -> %s and %s -> %s' % (
#                                     clause,parents1,v1,parents2,v2)
#                         continue
#                         # look for 3-cycles in one direction
#                         if v1 in parents2:
#                             for v3 in parents1:
#                                 if not (v3 > v1 and v3 > v2):
#                                     continue
#                                 for parents3 in self._family_scores[v3]:
#                                     if v2 in parents3:
#                                         clause = (-inv[v1,parents1],-inv[v2,parents2],-inv[v3,parents3])
#                                         kk += 1
#                                         hard_clauses.append(clause)
#                                         if comments:
#                                             print >> fobj, 'c Clause %s stops cycle: %s -> %s and %s -> %s and %s -> %s' % (
#                                                 clause,parents1,v1,parents2,v2,parents3,v3)
#                         if v2 in parents1:
#                             for v3 in parents2:
#                                 if not (v3 > v1 and v3 > v2):
#                                     continue
#                                 for parents3 in self._family_scores[v3]:
#                                     if v1 in parents3:
#                                         clause = (-inv[v1,parents1],-inv[v2,parents2],-inv[v3,parents3])
#                                         kk += 1
#                                         hard_clauses.append(clause)
#                                         if comments:
#                                             print >> fobj, 'c Clause %s stops cycle: %s -> %s and %s -> %s and %s -> %s' % (
#                                                 clause,parents1,v1,parents2,v2,parents3,v3)
                        
#         # dg = self.make_domain_graph()
# #         tmp = []
# #         for cycle in dg.enumerate_cycles(cycle_lim):
# #             print cycle
# #             clauses = [frozenset()]
# #             for i in range(len(cycle)-1):
# #                 parent, child = cycle[i], cycle[i+1]
# #                 new_clauses = []
# #                 for parent_set in self._family_scores[child]:
# #                     if parent in parent_set:
# #                         for clause in clauses:
# #                            new_clauses.append(clause | frozenset([-inv[child,parent_set]]))
# #                 clauses = new_clauses
# #             tmp.extend(clauses)

# #         for clause in tmp:
# #             for other in tmp:
# #                 if other != clause and other <= clause:
# #                     break
# #             else:
# #                 clause = tuple(clause)
# #                 hard_clauses.append(clause)
# #                 if comments:
# #                     cycle = ''
# #                     for atm in clause:
# #                         ch,parents = family_atoms[-atm]
# #                         cycle += '%s -> %s ' % (parents,ch)
# #                     print >>fobj, 'c Clause %s stops a cycle %s' % (clause,cycle)

                           
                        

#         # frontier = {frozenset():frozenset()}
# #         bn = ADG(self._variables)
        
# #         for i in range(cycle_lim):
# #             new_frontier = {}
# #             tmp_hard = []
# #             for atom, (child,parents) in family_atoms.items():
# #                 childset = frozenset([child])
# #                 atomset = frozenset([atom])
# #                 for clause, vs in frontier.items():
# #                     if child in vs:
# #                         continue
# #                     cclause = clause | atomset
# #                     try:
# #                         cbn = bn.copy()
# #                         for atm in cclause:
# #                             ch,parents = family_atoms[atm]
# #                             cbn.put_family(ch,parents)
# #                     except DirectedCycleError:
# #                         tmp_hard.append(cclause)
# #                     else:
# #                         new_frontier[cclause] = vs|childset
# #             for cclause in tmp_hard:
# #                 for oldclause in frontier:
# #                     if i > 1 and oldclause <= cclause:
# #                         print oldclause, cclause
# #                         break
# #                 else:
# #                     hard_clauses.append(tuple(cclause))
# #                     if comments:
# #                         cycle = ''
# #                         for atm in cclause:
# #                             ch,parents = family_atoms[atm]
# #                             cycle += '%s -> %s ' % (parents,ch)
# #                         print >>fobj, 'c Clause %s stops a cycle %s' % (cclause,cycle)

# #             frontier = new_frontier
                        
        
#         # print out header
#         print >>fobj, 'p wcnf %d %d' % (current_atom-1,len(hard_clauses)+len(soft_clauses))

#         # print out soft clauses

#         maxcost = 0
#         for clause in soft_clauses:
#             maxcost = max(maxcost,clause[0])
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'

#         hard_clause_cost = maxcost * len(self._family_scores)
#         if hard_clause_cost > 2147483647:
#             raise ValueError('%d too big' % hard_clause_cost)

#         # print out hard clauses
#         for clause in hard_clauses:
#             print >>fobj, '%d ' % hard_clause_cost,
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'
#         fobj.close()

#         self._wcnf_info['next_atom'] = current_atom
        
#         return current_atom

#     def write_cnf6(self,filename,scaling=1,comments=True):
#         """Use family atoms and a total order
#         """

#         from os.path import abspath
        
#         current_atom = 1
#         ord_atom = {}
#         fobj = open(filename,'w')
#         family_atoms = {}
#         self._wcnf_info['family_atoms'] = family_atoms
#         self._wcnf_info['filename'] = abspath(filename)
#         self._wcnf_info['scaling'] = scaling
#         self._wcnf_info['ord_atom'] = ord_atom
#         variables = self._variables
#         for i, v in enumerate(variables):
#             for w in variables[i+1:]:
#                 ord_atom[v,w] = current_atom
#                 if comments:
#                     print >>fobj, 'c Atom %d means %s < %s in total order' % (
#                         current_atom,v,w)
#                 current_atom += 1

#         hard_clauses = []
#         soft_clauses = []

#         for i, v in enumerate(variables):
#             for j, w in enumerate(variables[i+1:]):
#                 for z in variables[i+j+2:]:
#                     #print v,w,z
#                     # v < w, w < z => v < z
#                     clause = (-ord_atom[v,w],-ord_atom[w,z],ord_atom[v,z])
#                     hard_clauses.append(clause)
#                     if comments:
#                         print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
#                         clause, v,w,w,z,v,z)
#                     # v < z, z < w => v < w
#                     clause = (-ord_atom[v,z],ord_atom[w,z],ord_atom[v,w])
#                     hard_clauses.append(clause)
#                     if comments:
#                         print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
#                         clause, v,z,z,w,v,w)

#         # create atoms stating the existence of families
#         # and create weighted clauses to encode their scores
#         self._wcnf_info['printatoms'] = []
#         for child, pa_scores in self._family_scores.items():
#             first_ch_lit = current_atom
#             for parents, llh_score in pa_scores.items():
#                 family_atoms[current_atom] = (child,parents)
#                 if comments:
#                     print >>fobj, ('c Atom %d means that %s has parents %s' %
#                                    (current_atom,child,parents))
#                 for parent in parents:
#                     # parent before child in total order
#                     if parent < child: # parent before child lexicographically
#                         consequent = ord_atom[parent,child]
#                     else:
#                         consequent = -ord_atom[child,parent]
#                     hard_clauses.append((-current_atom,consequent))
#                     if comments:
#                         print >>fobj, 'c Clause %d %d: %s has parents %s => %s < %s' % (
#                             -current_atom,consequent,child,parents,parent,child)

#                     # soft constraint from data
#                 if comments:    
#                     print >>fobj, 'c Soft clause %d %d: since %s with parents %s has score %f and scaling is %d' % (
#                         int(-llh_score*scaling),-current_atom,child,parents,llh_score,scaling)
#                 soft_clauses.append((int(-llh_score*scaling),-current_atom))
#                 current_atom += 1
#             family_lits = range(first_ch_lit,current_atom)
#             if comments:
#                 print >>fobj, 'c Clause %s: since exactly one family for %s' % (
#                     ' '.join([str(x) for x in family_lits]), child)
#             hard_clauses.append(tuple(family_lits))

#             self._wcnf_info['printatoms'].extend(family_lits)

#         # print out header
#         print >>fobj, 'p wcnf %d %d' % (current_atom-1,len(hard_clauses)+len(soft_clauses))

#         # print out soft clauses

#         maxcost = 0
#         for clause in soft_clauses:
#             maxcost = max(maxcost,clause[0])
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'

#         hard_clause_cost = maxcost * len(self._family_scores)
#         if hard_clause_cost > 2147483647:
#             raise ValueError('%d too big' % hard_clause_cost)
#         if hard_clause_cost * len(hard_clauses) > 2147483647:
#             print 'Reducing hard clause cost to prevent possible overflow'
#             hard_clause_cost = 2147483647 / len(hard_clauses)


#         # print out hard clauses
#         for clause in hard_clauses:
#             print >>fobj, '%d ' % hard_clause_cost,
#             for lit in clause:
#                 print >>fobj, '%d ' % lit,
#             print >>fobj, '0'
#         fobj.close()

#         self._wcnf_info['next_atom'] = current_atom
        
#         return current_atom

#     def encode_arrows(self):
#         atom = self._next_atom
#         pa = {}
#         # atoms start at 1
#         pa_inv = [None]
#         n = len(self._variables)
#         for v in variables:
#             for w in variables:
#                 if v == w:
#                     continue
#                 pair = v,w
#                 pa[pair] = atom
#                 pa_inv.append(pair)
#                 atom += 1
#         self._arrow_atoms = pa
#         self._arrow_inv_atoms = pa_inv
#         self._next_atom = atom

#         if 'parents' in atomsets:
#             atom_store_parents = {}
#             for child in upper.vertices():
#                 for parent in upper.parents(child):
#                     atom_store_parents[parent,child] = current_atom
#                     current_atom += 1
#             atom_store['parents'] = atom_store_parents

#         if 'ancestors' in atomsets:
#             atom_store_ancestors = {}
#             for child in upper.vertices():
#                 for ancestor in upper.ancestors(child):
#                     atom_store_ancestors[ancestor,child] = current_atom
#                     current_atom += 1
#             atom_store['ancestors'] = atom_store_ancestors 

#         if 'families' in atomsets:
#             atom_store_families = {}
#             for child, pa_scores in self._family_scores.items():
#                 for parents in pa_scores:
#                     atom_store_families[parents,child] = current_atom
#                     current_atom +=1
#             atom_store['families'] = atom_store_families 

#         if 'total_order' in atomsets:
#              atom_store_total_order = {}
#              for i, first in enumerate(variables):
#                  for second in variables[i+1:]:
#                      atom_store_total_order[first,second] = current_atom
#                      current_atom += 1
#              atom_store['total_order'] = atom_store_total_order


#     def write_abbrev(self,adgs,fobj):
#         """Print out abbreviated representations of C{adgs}

#         For each variable (in order) print out its parent set as an
#         ordered list

#         @param adgs: Iterator/sequence of L{ADG} objects
#         @type adgs: Iterator/sequence
#         @param fobj: Writeable file
#         @type fobj: File
#         """
#         variables = self._variables
#         indx = self._indx
#         for adg in adgs:
#             for v in variables:
#                 print >>fobj, sorted([indx[v] for v in adg.parents(v)]) 

#     @staticmethod
#     def learn(data,cnf_filename,maxwalksat_filename,flags='',pa_lim=2,encode_arrows=False):
#         import os
#         prob = RandomADG(data.variables(),pa_lim=pa_lim)
#         prob.condition(data)
#         next_lit = prob.write_cnf(open(cnf_filename,'w'),encode_arrows)
#         init = open('initfile','w')
#         for atom in range(1,next_lit):
#             print >>init, -atom
#         init.close()
#         cmd = 'newmaxwalksat -init initfile -printatoms printatomsinitfile ' + flags + ' < ' + cnf_filename + " | grep '^C' | sort -u > " + maxwalksat_filename
#         print cmd
#         os.system(cmd)
#         return prob, open(maxwalksat_filename)



# class WCNFRandomADG(object):
#     """Random ADGs encoded for producing weighted CNF files
    
#     @ivar _pa_atom: Maps 2-tuples v,w of variables to the atom encoding that v is a
#     parent of w
#     @type _pa_atom: Dictionary
#     @ivar _an_atom: Maps 2-tuples v,w of variables to the atom encoding that v is an
#     ancestor of w
#     @type _an_atom: Dictionary
#     @ivar _family_atoms: Maps an integer encoding a family to a (child,parents) tuple where
#     parents is a frozenset
#     @type _family_atoms: Dictionary since first integer may not be 0.
#     @ivar _next_atom: The next available integer for encoding atoms
#     @type _next_atom: Int
#     @ivar _encode_arrows: whether arrows were encoded
#     @type _encode_arrows: Boolean
#     @ivar _scaling: scaling factor for converting scores to integers
#     @type _scaling: Number
#     @ivar _printatoms: list of atoms which will later be printed to a file and used as an
#     argument for the -printatoms flag to newmaxwalksat
#     @type _printatoms: List
#     """

#     def write_cnf(self,randomadg,atomsets=('parents','total_order'),
#                   acyclicity=1,direct_reversible=True,
#                   scaling=1):
#         """Encode C{randomadg} as a weighted CNF in DIMACS format

#         @param randomadg: The random ADG to encode
#         @type randomadg: L{RandomADG}
#         @param atomsets: Which graphical relations to encode as propositional atoms.
#         Only the following (given as strings) are currently supported:
#          parents: For each pa -> ch relation mentioned in the families in
#          the _family_scores attribute of self, create an atom asserting that pa is the parent of ch.
#          ancestors: For each unordered pair of vertices create an atom asserting that one
#          is an ancestor of the other, but only if the families stored in  the _family_scores attribute
#          allow this possibility.
#          families: For each family stored in the _family_scores attribute of self, create an atom
#          asserting that the family obtains.
#          total_order: For each ordered pair of vertices create an atom asserting that this pair
#          is lexicographically ordered in a total order.
#          cycle_atom: Create an atom asserting the existence of an atom.
#         Evidently, there is redundancy here and one would not want to represent all of these atoms.
#         The default gives the minimal number of atoms.
#         @type atomsest: A tuple of strings
#         @param acyclicity: If equals 1 cyclic digraphs are ruled out using hard clauses. Otherwise a number
#         between 0 and 1 is expected and the hard clauses are softened by multiplying their weights by C{acyclicity}.
#         The nature of the hard clauses depends on the atoms used in the encoding as specifed by C{atoms}
#         @type acyclicity: Number
#         @param direct_reversible: If using total_order, and an arrow is established to be reversible
#         then effect a lexicographic ordering
#         @type: direct_reversible: Boolean
#         @param scaling: How much to multiply scores by before making into integers
#         @type scaling: Numeric
#         """

#         atomsets = frozenset(atomsets)
#         allowable_atoms = frozenset(['parents','ancestors','families','total_order','cycle_atom'])
#         if not atomsets <= allowable_atoms:
#             raise ValueError("'atoms' argument can only contain values in %s" % allowable_atoms)

#         if ('parents' not in atomsets) and ('families' not in atomsets) and ('total_order' not in atomsets):
#             raise ValueError("Must have one of 'parents', 'families' or 'total_order'")

#         atom_store = {}
#         current_atom = 1
#         upper = randomadg.upper()
#         family_scores = randomadg.family_scores()
#         vertices = sorted(randomadg.vertices())

#         ######################################################################
#         # start of constructing atoms
#         ######################################################################

#         def get_related(atomset,child):
#             """Return suitable iterable"""
#             if atomset == 'parents':
#                 return upper.parents(child)
#             elif atomset == 'ancestor':
#                 return upper.ancestors(child)
#             elif atomset == 'families':
#                 return family_scores[child]
#             elif atomset == 'total_order':
#                 def tmp(x): return x < child
#                 return filter(tmp,vertices)

#         # atom_store['parents'][parent,child] is atom stating that 'parent' is parent of 'child'
#         # atom_store['ancestor'][ancestor,child] is atom stating that 'ancestor' is ancestor of 'child'
#         # atom_store['families'][parentset,child] is atom stating that 'parentset' are the parents of 'child'
#         # atom_store['total_order'][v1,v2] is atom stating that 'v1' and 'v2' are lexicographically ordered
#         #              in the associated total order. Only created for v1 < v2,
#         #              so  atom_store['total_order']['a','b'] would be created, but not
#         #                  atom_store['total_order']['b','a']

#         for atomset in atomsets:
#             dkt = {}
#             for vertex in vertices:
#                 for other in get_related(atomset,child):
#                     # current_atom iff the relevant relation holds between
#                     # vertex and other
#                     dkt[other,vertex] = current_atom
#                     current_atom += 1
#             atom_store[atomset] = dkt

             
#         if 'cycle_atom' in atomsets:
#             atom_store['cycle_atom'] = current_atom
#             current_atom += 1

#         ######################################################################
#         # end of constructing atoms
#         ######################################################################

#         ######################################################################
#         # start of constructing clauses
#         ######################################################################

#         clauses = []

#         def transitivity(dkt):
#             for (v1,v2), atom1 in dkt.items():
#                 for (v3,v4), atom2 in dkt.items():
#                     if v2 == v3:
#                         try:
#                             yield (-atom1, -atom2, dkt[v1,v4])
#                         except:
#                             pass
                    

#         # constraints effected by choosing parents.
#         # includes constraints stating that each variable must have some choice of parents

#         def implies(lhs,rhs):
#             """Generate implications between different relations"""
#             if lhs == 'families':
#                 def tmp(x): return x
#             elif lhs == 'parents':
#                 def tmp(x): return [x]
#             if (lhs != rhs) and rhs in 'parents', 'ancestors', 'total_order':
#                 for (parent_s,child), lhs_atom in atom_store[lhs].items():
#                     for parent in tmp(parent_s):
#                         if rhs == 'total_order' and child < parent:
#                             yield (-lhs_atom, -atom_store[rhs][child,parent])
#                         else:
#                             yield (-lhs_atom, atom_store[rhs][parent,child])

#         if 'families' in atomsets:
#             lhs = 'families'
#         elif 'parents' in atomsets:
#             lhs = 'parents'
#         for rhs in atomsets:
#             for clause in implies(lhs,rhs):
#                 clauses.append(clause)

#             for child, choices in effect_family_choice.items():
#                 clause = tuple(['hard']+choices)
#                 clauses.append(clause)
#                 if comments:
#                     print >>fobj, 'c Clause %s: %s must have one of these parent sets' % (
#                         clause,child)


#             # only combinations of parents stored in self._family_scores are permitted
#             # so effect this. And store the relevant clauses for later in fam_choice.

#             fam_choice = {}
#             for child, pa_scores in self._family_scores.items():
#                 fam_choice[child] = {}
#                 for parents in pa_scores:
#                     # this clause broken if these parents chosen
#                     clause = [-atom_store_parents[parent,child] for parent in parents]
#                     # now add literals so that no other clauses are broken if this one is
#                     also = set()
#                     for other_parents in pa_scores:
#                         if other_parents >= parents:
#                             also.update(other_parents - parents)
#                     for other_parent in also:
#                         clause.append(atom_store_parents[other_parent,child])
#                     fam_choice[child][parents] = tuple(clause)

#                 # The function neg_cnf constructs a CNF asserting that not all the clauses
#                 # supplied to it are true.
#                 #print 'Ruling out illegal parent choices for ', child
#                 for neg_clause in neg_cnf(fam_choice[child].values()):
#                     #print neg_clause
#                     clause = tuple(['hard']+sorted(neg_clause))
#                     clauses.append(clause)
#                     if comments:
#                         print >> fobj, 'c %s part of CNF ruling out illegal parent choices for %s' % (
#                             clause,child)
#                 #print 'Done'

#         # case where only total_order is used.
#         else:

#             def post_clauses(sorted_parents,order,maxcost):

                
#                 # if order contradicts a parent-child relationship in first
#                 # parent set, then move on to the next one
                
#                 for parent in sorted_parents[0]:
#                     if (child,parent) in order:
#                         return post_clauses(sorted_parents[1:],order,maxcost)

#                 # if a constraint is missing then add it
#                 # and also fire off other branch

#                 for parent in sorted_parents[0]:
#                     if (parent,child) not in order:
#                         maxcost = post_clauses(sorted_parents,order | frozenset([(parent,child)]),maxcost)
#                         return post_clauses(sorted_parents[1:],order | frozenset([(child,parent)]),maxcost)
                    
#                 # all constraints must be met, so sorted_parents[0]
#                 # consistent with order

#                 llh_score = pa_scores[sorted_parents[0]]
#                 cost = int(-llh_score * scaling)
#                 maxcost = max(maxcost,cost)
#                 clause = [cost]
#                 for (before,after) in order:
#                     if before < after:
#                         clause.append(-atom_store_total_order[before,after])
#                     else:
#                         clause.append(atom_store_total_order[after,before])
#                 clauses.append(tuple(clause))
#                 if comments:    
#                     print >>fobj, 'c Soft clause %s: since with these order constraints: %s, these parents %s will be chosen for %s and they have score %f and scaling is %d' % (clause,order,sorted_parents[0],child,llh_score,scaling)
#                 return maxcost

#             maxcost = 0
#             for child, pa_scores in self._family_scores.items():
#                 def tmpfun(parents): return -pa_scores[parents]
#                 sorted_parents = tuple(sorted(pa_scores.keys(),key=tmpfun))
#                 maxcost = post_clauses(sorted_parents,frozenset(),maxcost)
                
#         # transitivity of ancestor relation 

#         if 'ancestor' in atomsets:
#             for (ancestor, child), ancestor_atom in atom_store_ancestors.items():
#                 for middle in variables:
#                     try:
#                         clause = ('hard',-atom_store_ancestors[ancestor,middle],
#                                   -atom_store_ancestors[middle,child],
#                                   ancestor_atom)
#                         clauses.append(clause)
#                         if comments:
#                             print >>fobj, 'c Clause %s: %s ->> %s & %s ->> %s => %s ->> %s' % (
#                                 clause,ancestor,middle,middle,child,ancestor,child)
#                     except KeyError:
#                         pass # not all triples effect a transitivity relation

#         # soft clauses for choices of parent sets


#         if 'families' in atomsets:
#             def get_clause(child,parents): return (-atom_store_families[parents,child],)
#         elif 'parents' in atomsets:
#             def get_clause(child,parents): return fam_choice[child][parents]
#         if ('families' in atomsets) or ('parents' in atomsets):
#             maxcost = 0
#             for child, pa_scores in self._family_scores.items():
#                 for parents, llh_score in pa_scores.items():
#                     cost = int(-llh_score*scaling)
#                     maxcost = max(maxcost,cost)
#                     clause = (cost,) + get_clause(child,parents)
#                     clauses.append(clause)
#                     if comments:    
#                         print >>fobj, 'c Soft clause %s: since %s with parents %s has score %f and scaling is %d' % (
#                             clause,child,parents,llh_score,scaling)
#                     if direct_reversible:
#                         for parent in parents:
#                             for p_parents in self._family_scores[parent]:
#                                 if p_parents == parents - frozenset([parent]): # is reversible
#                                     if 'families' in atomsets:
#                                         if parent < child: # 'out of default order'
#                                             clause = ('hard',-atom_store_families[parents,child],
#                                                       -atom_store_families[p_parents,parent])
#                                         else:
#                                             continue
#                                     else:
#                                         clause = (
#                                             ('hard',) +
#                                             fam_choice[child][parents] +
#                                             fam_choice[parent][p_parents] +
#                                             (atom_store_total_order[tuple(sorted([child,parent]))],)
#                                             )
#                                     if comments:
#                                         print >>fobj, 'c Clause %s since %s -> %s parent is reversible when %s has parents %s and %s has parents %s' % (clause,parent,child,child,parent,parent,p_parents)

#         # constraints ruling out cycles may be soft
#         # so compute weighting here if this is the case
        
#         def compute_hard_clause_cost():
#             hard_clause_cost = maxcost * len(self._family_scores)
#             if hard_clause_cost > 2147483647:
#                 raise ValueError('%d too big' % hard_clause_cost)
#             if hard_clause_cost * len(clauses) > 2147483647:
#                 print 'Reducing hard clause cost to prevent possible overflow'
#                 hard_clause_cost = 2147483647 / len(clauses)
#             return hard_clause_cost

        
#         if acyclicity == 1:
#             def softened_clause(clause): return ('hard',) + clause
#         else: # rest are all soft clauses
#             hard_clause_cost = compute_hard_clause_cost()
#             def softened_clause(clause): return (acyclicity * hard_clause_cost,) + clause

#         # no cycles

#         if 'ancestor' in atomsets:
#             for (ancestor, child), ancestor_atom in atom_store_ancestors.items():
#                 if ancestor < child:
#                     try:
#                         if 'cycle_atom' in atomsets:
#                             clause = softened_clause((-atom_store_ancestors[ancestor,child],
#                                       -atom_store_ancestors[child,ancestor],
#                                       atom_store_cycle_atom))
#                             clauses.append(clause)
#                             if comments:
#                                 print >>fobj, 'c Clause %s: %s ->> %s & %s ->> %s => a cycle' % (
#                                     clause,ancestor,child,child,ancestor)
#                         else:
#                             clause = softened_clause((-atom_store_ancestors[ancestor,child],
#                                       -atom_store_ancestors[child,ancestor]))
#                             clauses.append(clause)
#                             if comments:
#                                 print >>fobj, 'c Clause %s: not (%s ->> %s & %s ->> %s)' % (
#                                     clause,ancestor,child,child,ancestor)
#                     except KeyError:
#                         pass # may be missing ancestor-child pairs

#         # transitivity of total order relation (implicitly no cycles)

#         def unneeded(clause):
#             """At least 2 lits must conflict with a consequent
#             of a family choice
#             """
#             return False # temporary alteration
#             one_found = False
#             for lit in clause[1:]:
#                 if -lit in consequents:
#                     if one_found:
#                         return determined
#                     else:
#                         one_found = True
#                 else:
#                     determined = -lit
#             return 0
        
#         if 'total_order' in atomsets:
#             for i, third in enumerate(variables):
#                 for j, second in enumerate(variables[:i]):
#                     for first in variables[:j]:
#                         if 'cycle_atom' in atomsets:
#                             clause = softened_clause((atom_store_total_order[first,second],
#                                                       atom_store_total_order[second,third],
#                                                       -atom_store_total_order[first,third],
#                                                       atom_store_cycle_atom))
#                             if unneeded(clause):
#                                 continue
#                             clauses.append(clause)
#                             if comments:
#                                 print >> fobj, 'c Clause %s since %s < %s & %s < %s & %s < %s is a cycle' % (
#                                     clause, first, second, second, third, third, first)
#                             clause = softened_clause((-atom_store_total_order[first,second],
#                                                       -atom_store_total_order[second,third],
#                                                       atom_store_total_order[first,third],
#                                                       atom_store_cycle_atom))
#                             if unneeded(clause):
#                                 continue
#                             clauses.append(clause)
#                             if comments:
#                                 print >> fobj, 'c Clause %s since %s < %s & %s < %s & %s < %s is a cycle' % (
#                                     clause, second, first, third, second, third, first)
#                         else:
#                             clause = softened_clause((-atom_store_total_order[first,second],
#                                                       -atom_store_total_order[second,third],
#                                                       atom_store_total_order[first,third]))
#                             if unneeded(clause):
#                                 print  'c Clause %s since %s < %s & %s < %s => %s < %s' % (
#                                     clause, first, second, second, third, first, third)
#                                 continue
#                             clauses.append(clause)
#                             if comments:
#                                 print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
#                                     clause, first, second, second, third, first, third)
#                             clause = softened_clause((atom_store_total_order[first,second],
#                                                       atom_store_total_order[second,third],
#                                                       -atom_store_total_order[first,third]))
#                             if unneeded(clause):
#                                 print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
#                                     clause, second, first, third, second, third, first)
#                                 continue
#                             clauses.append(clause)
#                             if comments:
#                                 print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
#                                     clause, second, first, third, second, third, first)
            
#         ######################################################################
#         # end of constructing clauses
#         ######################################################################

#         # print out header
#         print >>fobj, 'p wcnf %d %d' % (current_atom-1,len(clauses))

#         if acyclicity == 1:
#             hard_clause_cost = compute_hard_clause_cost()

#         # print out clauses
#         for clause in clauses:
#             if clause[0] == 'hard':
#                 print >>fobj, '%d ' % hard_clause_cost,
#                 for lit in clause[1:]:
#                     print >>fobj, '%d ' % lit,
#             else:
#                 for lit in clause:
#                     print >>fobj, '%d ' % lit,
#             print >>fobj, '0'
#         fobj.close()

#         self._wcnf_info['next_atom'] = current_atom
        
#         return current_atom
