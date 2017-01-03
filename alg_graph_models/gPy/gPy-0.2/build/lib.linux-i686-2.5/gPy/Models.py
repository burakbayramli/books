"""
Factored representations of probability distributions and probability models

@var _version: Version of this module
@type _version: String
"""

from Variables import SubDomain
from Hypergraphs import Hypergraph, ReducedHypergraph, SimpleHypergraph, ReducedJoinForest
from Graphs import ADG, DiForest, MutilatedADG, EssentialGraph
from IO import GraphCanvas
from Utils import emptyset, pretty_str_set, member
from Parameters import Factor, CPT
from Data import CompactFactor
import Tkinter, operator

_version = '$Id: Models.py,v 1.14 2008/10/07 09:11:55 jc Exp $'

class _AbsM(SubDomain):
    """Abstract model class

    Defined by a hypergraph and specification of values for the variables.
    Represents the set of all probability distributions which have a factored representation whose
    hypergraph is the specifed one.

    Since the hypergraph is not required to be simple it is not useful to actually create objects
    of this class, hence it is an abstract class.
    The most general model class is that of log-linear models (LLM objects, not yet implemented) where the
    associated hypergraph is required to be simple.
    
    @ivar _hypergraph: The hypergraph associated with the model. There is exactly
    one hyperedge for each factor. The hyperedge for a given factor is just the set of that
    factor's variables (implemented as a frozenset). If the hypergraph is non-simple, then hyperedges
    can be repeated.
    @type _hypergraph: L{Hypergraph} object
    @ivar _factors: A mapping from each distinct hyperedge in the associated hypergraph to a list of 
    factors having the variables of the hyperedge. 
    @type _factors: Dictionary
    """

    def __init__(self,factors=(),domain=None,new_domain_variables=None,
                 must_be_new=False,check=False):
        """Initialise a hierarchical model

        Each factor has its domain expanded to be the domain of the model, if necessary.
        @param factors: The factors in the hierarchical model.
        (Alternatively an existing object of class L{FR} (or its subclasses), in
        which case C{self} has identical attributes.)
        @type factors: Sequence, each element of which is a L{Parameters.Factor} or
        L{Variables.SubDomain} object. (Alternatively an object of class L{FR}
        or one of its subclasses.)
        @param domain: A domain for the model.
        If None and all C{factors} have the same domain then this domain is used.
        If None and all C{factors} do not have the same domain then the internal default domain is used.
        @type domain: L{Variables.Domain} or None
        @param new_domain_variables: A dictionary containing a mapping from any new
        variables to their values. C{domain} is updated with these values
        @type new_domain_variables: Dict or None
        @param must_be_new: Whether domain variables in C{new_domain_variables} have
        to be new
        @type must_be_new: Boolean
        @param check: Whether to check that all variables exist in C{domain}
        @type check: Boolean
        @raise VariableError: If a variable in C{new_domain_variables}
        already exists with values different from
        its values in C{new_domain_variables};
        Or if C{must_be_new} is set and the variable already exists.
        Or if C{check} is set and a variable in C{variables} is not in the domain
        """
        if isinstance(factors,FR):
            self.__dict__ = factors.__dict__
            return
        self._factors = {}
        hypergraph = Hypergraph()
        variables = set()
        for factor in factors:
            hyperedge = factor.variables()
            variables.update(hyperedge)
            hypergraph.add_hyperedge(hyperedge)
            self._add_factor_to_dict(hyperedge,factor)
        if domain is None and len(factors) > 0:
            first_factor_domain = factors[0]._domain
            for factor in factors[1:]:
                if factor._domain is not first_factor_domain:
                    break
            else:
                domain = factors[0]
        SubDomain.__init__(self,variables,domain,new_domain_variables,must_be_new,check)
        for factor in factors:
            self.common_domain(factor)
        self._hypergraph = hypergraph

    def __getitem__(self,hyperedge):
        """Return the factor(s) (not a copy) whose variables are C{hyperedge}

        @param hyperedge: The variables of the sought factor
        @type hyperedge: Iterable
        @return: The factor(s)
        @rtype: A L{Parameters.Factor} object (for simple) or a list of such objects (non-simple)
        @raise KeyError: If there is no factor with these variables
        """
        return self._factors[frozenset(hyperedge)]

    def __iter__(self):
        """Return an iterator over the factors in the model

        To allow C{for factor in model: ...} constructions.
        @return: An iterator over the factors in the model
        @rtype: Iterator
        """
        fs = []
        for factorlist in self._factors.values():
            fs.extend(factorlist)
        return iter(fs)

    def __len__(self):
        """Return the number of factors in the model

        @return: The number of factors in the model
        @rtype: Int
        """
        return len(self._hypergraph)

    def __repr__(self):
        return 'FR(%s,None,%s)' % (self._factors.values(),
                                   self._domain)

    def __setitem__(self,hyperedge,factor):
        """Set the factor whose variables are C{hyperedge} to factor

        @param hyperedge: The variables of the factor
        @type hyperedge: Iterable
        @param factor: Factor which will replace existing factor(s)
        @type factor: L{Parameters.Factor}
        @raise KeyError: If there is no factor with these variables
        """
        hyperedge = frozenset(hyperedge)
        if hyperedge != factor.variables():
            raise ValueError('Can only replace factors with other factors with the same variables: %s, %s' % (hyperedge, factor.variables()))
        self._put_factor_to_dict(hyperedge,factor)


    def __str__(self):
        """Print each factor separately

        Use lexicographical ordering on factor variables

        @return: Pretty representation of a L{FR}
        @rtype: String
        """
        out = ''
        tmp = []
        for hyperedge,factor in self.items():
            l = list(hyperedge)
            l.sort()
            tmp.append((l,factor))
        tmp.sort()
        for l, factor in tmp:
            out += str(factor)
            out += '\n'
        return out

    def __div__(self,other):
        """Return the result of dividing a hierarchical model by a scalar

        A randomly chosen factor is divided by the scalar

        The returned value shares the same domain as C{self}. To avoid this make a deep copy and do
        in-place division.
        
        @param other: The scalar
        @type other: float or int
        """
        return self.copy().__idiv__(other)


    def __idiv__(self,other):
        """Divide a hierarchical model by a scalar

        A randomly chosen factor is divided by the scalar
        
        @param other: The scalar
        @type other: float or int
        """
        for factor in self:
            factor /= other
            break
        return self


    def __imul__(self,other):
        """Multiply a hierarchical model by a factor, scalar or another L{FR}

        @param other: The factor or L{FR} object to be multiplied in
        @type other: L{Factor} of L{FR} object
        """
        if isinstance(other,Factor):
            self._add_factor(other)
        elif isinstance(other,FR):
            for factor in other:
                self._add_factor(factor)
        else:
            for factor in self:
                factor *= other
                break
        return self

    def __mul__(self,other):
        """Return the result of multiplying a hierarchical model by a factor, scalar or another L{FR}

        The returned value shares the same domain as C{self}. To avoid this make a deep copy and do
        in-place multiplication.

        @param other: The factor or L{FR} object to be multiplied in
        @type other: L{Factor} of L{FR} object
        """
        return self.copy().__imul__(other)

    def add_ident(self,hyperedge):
        """Add an ident factor for a hyperedge to a hierarchical model

        Typically C{hyperedge} will come from a L{Hypergraph} model.

        An ident factor maps all instantiations to 1.0
        @param hyperedge: The variables for the factor
        @type hyperedge: Iterable
        """
        hyperedge = frozenset(hyperedge)
        self._hypergraph.add_hyperedge(hyperedge)
        self._add_factor_to_dict(hyperedge,Factor(hyperedge))
        self._variables = frozenset(self._hypergraph.vertices())

    def condition(self,condition,keep_class=False):
        """Alter a distribution by effecting the restriction on
        variables given by C{condition}

        This alters the model's domain. Make a copy with C{copy_domain=True}
        if the original domain will be needed
        @param condition: Dictionary of the form {var1:values1,var2:values2..}
        Each value of this dictionary must be an iterable
        @type condition: Dict
        @return: The conditioned model
        @param keep_class: TODO
        @type keep_class: Boolean
        @rtype: Same as C{self}
        @raise KeyError: If a variable is used that is not in the model
        @raise ValueError: If a value is used that is not a possible value of
        the variable it is attached to
        """
        self._check_condition(condition)
        # change data in each factor
        for factor in self:
            factor.data_restrict(condition,keep_class)
        # change stored values
        self.change_domain_variables(condition)
        return self

    def copy(self,copy_domain=False):
        """Return a deep copy of a hierarchical model

        @param copy_domain: If true C{self}'s domain is copied, otherwise the copy
        shares C{self}'s domain
        @type copy_domain: Boolean
        @return: A copy of C{self}
        @rtype: Same type as C{self}
        """
        cp = SubDomain.copy(self,copy_domain)
        cp.__class__ = self.__class__
        cp._hypergraph = self._hypergraph.copy()
        factors = {}
        for hyperedge, factorlist in self._factors.items():
            nw = []
            for factor in factorlist:
                fc = factor.copy()
                cp.common_domain(fc)
                nw.append(fc)
            factors[hyperedge] = nw
        cp._factors = factors
        return cp

    def cpt(self,child,parents=()):
        """Return a conditional probability table for specified child and parents

        Return the probability distribution of C{child} conditional on C{parents}
        
        @param child: Child of the CPT
        @type child: String
        @param parents: Parents of C{child} in the CPT
        @type parents: Sequence
        @return: The specified CPT
        @rtype: L{Parameters.CPT}
        """
        cp = self.copy()
        vs = tuple(parents) + ((child),)
        cp.marginal(vs)
        joint = 1
        for f in cp:
            joint *= f
        return CPT(joint,child,cpt_force=True)
        
    
    def eliminate_variable(self,variable,trace=False):
        """Alter a factored representation by summing out a variable

        Removes one or more factors.
        @param variable: The variable to eliminate
        @type variable: String
        @raise KeyError: If C{variable} is not in the model
        """
        hyperedges = self._hypergraph.star(variable)
        if variable in self._instd:
            vset = set([variable])
            for hyperedge in hyperedges:
                # drop the variable from the factor, #hmm unnecessary multiplication
                nf = SubDomain.marginalise_away(
                    self.factor(hyperedge),vset)
                # remove old factor
                self.remove(hyperedge)
                # add in new one
                self *= nf
            if trace:
                return None, None, hyperedges
            else:
                return
        prod_factor = 1
        for hyperedge in hyperedges:
            prod_factor *= self.factor(hyperedge)
            self.remove(hyperedge)
        message = prod_factor.sumout([variable])
        self *= message
        if trace:
            return prod_factor.variables(), message.variables(), hyperedges

    def factor(self,hyperedge):
        """Return the factor produced by multiplying all factors with variables C{hyperedge}

        @param hyperedge: Set of variables
        @type hyperedge: Iterable
        @return: Product of all factors with variables C{hyperedge}
        @rtype: L{Parameters.Factor} object
        @raise KeyError: if no factor has C{hyperedge} as variables.
        """
        factors = self._factors[frozenset(hyperedge)]
        prod = factors[0]
        for f in factors[1:]:
            prod *= f
        return prod

    def factors(self):
        """
        Return a list of the factors in the model

        @return: A list of the factors in the model
        @rtype: List
        """
        fs = []
        for factorlist in self._factors.values():
            fs.extend(factorlist)
        return fs

    def factors_containing_variable(self,variable):
        """Return a list of factors containing C{variable}

        If non-simple, then a list of lists of factors is returned

        @param variable: A variable
        @type variable: Immutable (usually string)
        @return: A list of factors containing C{variable}
        @rtype: List
        """
        fs = []
        for hyperedge in self._hypergraph[variable]:
            fs.extend(self._factors[hyperedge])
        return fs
                

    def items(self):
        """Return sequence of C{factor.variables(),factor} pairs
        for each factor in the model.

        @return: Sequence of C{factor.variables(),factor} pairs
        for each factor in the model
        @rtype: List
        """
        itms = []
        for hyperedge, factorlist in self._factors.items():
            for factor in factorlist:
                itms.append((hyperedge,factor))
        return itms
    
    def gui_display(self,parent,colours=None):
        """Display a GUI widget for displaying a model
    
        @param parent: A widget into which the GUI is placed.
        @type parent: Some suitable Tk object.
        @param colours: Mapping from hyperedges to colours
        @type colours: Dictionary
        """
        if colours is None:
            colours = {}
        gui = Tkinter.Frame(parent)
        gui.pack()
        top = Tkinter.Frame(gui)
        top.pack()
        fgs = []
        for hyperedge, factor in self.items():
            fgs.append(factor.gui_main(top,edit=False,bg=colours.get(hyperedge,'grey')))
            fgs.append(Tkinter.Label(top,text='*'))
        fgs.pop()
        for widget in fgs:
            widget.pack(side=Tkinter.LEFT)
        bottom = Tkinter.Frame(gui)
        bottom.pack()
        for (txt,cmd) in [('Done',gui.destroy),
                          ('Quit', parent.destroy)]:
            button = Tkinter.Button(bottom,text=txt,command=cmd)
            button.bind('<Return>', lambda event: cmd())
            button.pack(side=Tkinter.LEFT)

    def hypergraph(self):
        """
        Return (a copy of) the hypergraph associated with the model

        @return: (A copy of) the hypergraph associated with the model
        @rtype: L{Hypergraph}
        """
        return self._hypergraph.copy()

    def inc_from_rawdata(self,rawdata):
        """
        Increment C{self} with counts directly from C{rawdata}

        OK for parameter fitting not for structure learning
        @param rawdata: A tuple like that returned by L{IO.read_csv}.
        @type rawdata: Tuple
        @raise IndexError: If C{self} has a variable missing from C{rawdata}
        """
        variables,records = rawdata[2:]
        factor_variables_info = []
        factors = self.factors()
        for factor in factors:
            factor_variables_info.append(factor.get_variables_info(variables))
        for record in records:
            for i, factor in enumerate(factors):
                factor.inc_from_record(factor_variables_info[i],record)

    def interaction_graph(self):
        """Return the interaction graph for a model

        The interaction graph contains an edge for any pair of variables
        which are members of a common factor.
        @return: The interaction graph
        @rtype: L{Graphs.UGraph}
        """
        return self._hypergraph.two_section()

    def is_simple(self):
        """Whether the model is simple

        @return: Whether the model is simple
        @rtype: Boolean
        """
        return self._hypergraph.is_simple()

    def make_decomposable(self,elimination_ordering=None):
        """Return a decomposable model using an elimination ordering

        If no C{elimination_ordering} is given
        L{Hypergraphs.ReducedHypergraph.maximum_cardinality_search} is used to
        provide one.

        Returned object shares many attributes with C{self}. Use a copy of self,
        if it still needed as an independent object.

        @param elimination_ordering: Order in which to eliminate variables
        @type elimination_ordering: Sequence
        @return: A decomposable model
        @rtype: L{DFR}
        """
        
        dg, destination = self.hypergraph().make_decomposable2(elimination_ordering)
        fs = {}
        for new_hyperedge in dg:
            fs[new_hyperedge] = Factor(new_hyperedge,domain=self)
        for old_hyperedge, new_hyperedge in destination.items():
            fs[new_hyperedge] *= self.factor(old_hyperedge)
        dm = DFR()
        dm.__dict__ = self.__dict__
        dm._hypergraph = dg
        dm._factors = fs
        return dm
        
#     def make_reduced(self):
#         """Return a reduced model by absorbing redundant factors

#         Returned object shares many attributes with C{self}. Use a copy of self,
#         if it still needed as an independent object.

#         @return: A reduced model
#         @rtype: L{RFR}
#         """
#         destinations = self._hypergraph.make_reduced()
#         for redund, supersets in destinations.items():
#             superset = member(supersets)
#             self._factors[superset] *= self._factors[redund]
#             del self._factors[redund]
#         rm = RFR()
#         rm.__dict__ = self.__dict__
#         try:
#             del rm._adg
#         except NameError:
#             pass
#         return rm
        

    def marginal(self,variables):
        """Alter a model to represent the marginal distibution
        on C{variables}

        Marginal only represented up to normalisation

        If C{self} is a BN and C{variables} is an ancestral set then C{self} remains a BN otherwise
        it becomes a general hierarchical model.
        """
        self.sumout(self.variables().difference(variables))

    def marginal_factor(self,variables):
        """Return the marginal distibution
        on C{variables} as a L{Parameters.Factor}

        Marginal is properly normalised

        """
        cp = self.copy()
        cp.sumout(self.variables().difference(variables))
        # used to be ..., why did Charles change
        # return cp[variables].normalised()
        return reduce(operator.mul, cp).normalised()

    def marginalise_away(self,variables,naive=True):
        """Alter a factored representation by summing out variables

        TODO: If we just want a marginal, no need to bother altering self
        
        @param variables: Ordered variables to eliminate
        @type variables: Sequence type
        @param naive: If C{True} the variables are summed out in the order given
        by C{variables}. If C{False} conditional independence and the number of factors
        a variable is in is used to construct a more intelligent order.
        @type naive: Boolean
        @return: The altered C{self}
        @rtype: Class of C{self}
        """
        if not naive:
            # remove factors all of whose variables are conditionally
            # independent of those remaining
            insted = self._instd
            remaining = self.variables() - set(variables)
            hypergraph = self._hypergraph
            reachable = set(hypergraph.reachable(remaining-insted,insted))
            reachable.update(remaining)
            for hyperedge in hypergraph.hyperedges():
                if not (hyperedge & reachable):
                    self.remove(hyperedge)

            # only concerned with surviving variables
            variables = self.variables().intersection(variables)
            
            # choose fixed ordering depending on number of hyperedges a variable is in
            def var_sort(v):
                return self.num_factors_containing_variable(v)
            variables = sorted(variables,key=var_sort)

        for variable in variables:
            self.eliminate_variable(variable)
        return self

    def markov_blanket(self,variable):
        """Return the Markov blanket for C{variable}

        @param variable: Variable in the model
        @type variable: Immutable
        @return: Markov blanket for C{variable}
        @rtype: Set
        @raise KeyError: If C{variable} is not in the model
        """
        return self._hypergraph.neighbours(variable)

    def num_factors_containing_variable(self,variable):
        """Return the number of distincet factors containing a given variable
        """
        return self._hypergraph.star_size(variable)

    def remove(self,hyperedge):
        """Remove a factor or factors from a hierarchical model

        All factors with variables C{hyperedge} will be removed

        @param hyperedge: The variables of the factor
        @type hyperedge: Iterable
        @raise KeyError: If no factor with these variables exists
        """
        hyperedge = frozenset(hyperedge)
        del self._factors[hyperedge]
        self._hypergraph.remove_hyperedge(hyperedge)
        self._variables = frozenset(self._hypergraph.vertices())

    def red(self):
        """Reduce the model, returning any distinct redundant hyperedges

        Any factor all of whose variables are contained in another is removed
        @return: Redundant hyperedges
        @rtype: List
        """
        self.simplify()
        reds = self._hypergraph.redundant_hyperedges()
        for hyperedge, supersets in reds.items():
            absorbing_factor = self.some_factor(supersets.pop())
            absorbing_factor *= self.factor(hyperedge)
            self.remove(hyperedge)
        reds = reds.keys()
        if emptyset in self._hypergraph:
            reds.append(emptyset)
            self.remove_hyperedge(emptyset)
        return reds

    def reduced(self):
        """Test whether a model is reduced

        @return: Whether a model is reduced
        @rtype: Boolean
        """
        return self._hypergraph.is_reduced()

    def simplify(self):
        """Simplify the model

        so that there are no factors with the same variable sets
        """
        for hyperedge, factorlist in self._factors.items():
            nfl = factorlist[0]
            for f in factorlist[1:]:
                nfl *= f
            self._factors[hyperedge] = [nfl]

    def some_factor(self,hyperedge):
        """Return an arbitrary factor with variables C{hyperedge}

        @param hyperedge: Variable set
        @type hyperedge: Iterable
        @return: Factor
        @rtype: L{Parameters.Factor} object
        @raise KeyError: If no factor with C{hyperedge} as varaibles exists
        """
        self._factors[frozenset(hyperedge)][0]
            
    def sumout(self,variables):
        """Sum out (marginalise away) variables using maximum cardinality

        C{variables} may be altered

        @param variables: Variables to sum out
        @type variables: Iterable
        @return: The marginal model
        @rtype: Same as C{self}
        """
        if not variables:
            return
        min = len(self._hypergraph)
        for variable in variables:
            n = self._hypergraph.star_size(variable)
            if n == 1:
                best = variable
                break
            else:
                if n <= min:
                    min = n
                    best = variable
        self.eliminate_variable(best)
        if not isinstance(variables,set):
            variables = set(variables)
        variables.remove(best)
        self.sumout(variables)
        return self

    variable_elimination = marginalise_away
        
    def variable_elimination_trace(self,variables):
        """As L{variable_elimination} but also return the corresponding
        L{Graphs.DiForest} object

        If all variables are summed out then the relevant scalar is returned
        @param variables: Ordered variables to eliminate
        @type variables: Sequence type
        @return: C{None} usually. The relevant scalar if all variables are
        summed out
        @rtype: C{None} or C{Float}
        """
        junction_forest = DiForest()
        message_produced = {}
        for variable in variables:
            cluster, new_message, messages_used = self.eliminate_variable(variable,True)
            junction_forest.add_vertex(cluster)
            for old_cluster, old_message in message_produced.items():
                if old_message in messages_used:
                    junction_forest.add_arrow(old_cluster,cluster)
                    del message_produced[old_cluster]
            message_produced[cluster] = new_message
        return junction_forest


    def z(self):
        """Return the sum of values associated with each full joint instantiation

        This is the partition function. Result is generally not 1.0 since models may
        represent distributions only up to normalisation. An expensive operation.
        @return: The sum of values associated with each full joint instantiation
        @rtype: Float
        @todo: Use sensible ordering of variables to eliminate
        """
        tmp = self.copy() 
        tmp.variable_elimination(tmp._variables)
        return tmp.factor(emptyset).z()

    def zero(self):
        """Set all values in all factors to zero
        """
        for factor in self:
            factor.zero()

    def _add_factor(self,factor):
        """Multiply a hierarchical model by a factor

        @param factor: The factor to be multiplied in
        @type factor: L{Factor}
        """
        self.common_domain(factor)
        hyperedge = factor.variables()
        self._variables |= hyperedge
        self._hypergraph.add_hyperedge(hyperedge)
        self._add_factor_to_dict(hyperedge,factor)
        #why is this done twice!?
        self._variables = frozenset(self._hypergraph.vertices())

    def _add_factor_to_dict(self,hyperedge,factor):
        """Add a factor to the _factors dictionary

        @param hyperedge: The hyperedge for the factor
        @type hyperedge: Frozenset
        @param factor: The factor to be added
        @type factor: L{Factor}
        """
        try:
            self._factors[hyperedge].append(factor)
        except KeyError:
            self._factors[hyperedge] = [factor]

    def _check_condition(self,condition):
        for variable, values in condition.items():
            if variable not in self._variables:
                raise KeyError('%s not a variable of this model' % variable) 
            for value in values:
                if value not in self._domain[variable]:
                    raise ValueError(
                        "%s has values %s. '%s' is not one of them" %
                        (variable, tuple(self._domain[variable]), value))

    def _put_factor_to_dict(self,hyperedge,factor):
        """Put a factor in the _factors dictionary

        Overwriting any previous entry

        @param hyperedge: The hyperedge for the factor
        @type hyperedge: Frozenset
        @param factor: The factor to be put
        @type factor: L{Factor}
        """
        self._factors[hyperedge] = [factor]

class FR(_AbsM):
    """Factored representations of probability distributions

    Probability distributions represented by a set of factors.
    The distribution is equal to the product of these factors.
    Each factor is a L{Parameters.Factor}.
    """
    pass

class SFR(FR):
    """Factored representations of probability distributions (associated hypergraph is simple)

    Probability distributions represented by a set of factors.
    The distribution is equal to the product of these factors.
    Each factor is a L{Parameters.Factor}.

    No two factors have the same variables.
    If factors were (A,B), (A,B), (B,C) then would NOT be simple.

    @ivar _hypergraph: The hypergraph associated with the model. There is exactly
    one hyperedge for each factor. The hyperedge for a given factor is just the set of that
    factor's variables (implemented as a frozenset). 
    @type _hypergraph: L{SimpleHypergraph} object
    @ivar _factors: A mapping from each hyperedge in the associated hypergraph to its associated factor,
    who will have the hyperedge as its variables.
    @type _factors: Dictionary
    """
    def __init__(self,factors=(),domain=None,new_domain_variables=None,
                     must_be_new=False,check=False):
        FR.__init__(self,factors,domain,new_domain_variables,must_be_new,check)
        self._hypergraph = SimpleHypergraph(self._hypergraph)

    def __iter__(self):
        """Return an iterator over the factors in the model

        To allow C{for factor in model: ...} constructions.
        @return: An iterator over the factors in the model
        @rtype: Iterator
        """
        return self._factors.itervalues()

    def _add_factor(self,factor):
        """Multiply a simple hierarchical model by a factor

        If a factor with the same variables already exists,
        then this existing factor is multiplied by C{factor} to maintain
        simplicity.

        @param factor: The factor to be multiplied in
        @type factor: L{Factor}
        """
        try:
            self._factors[factor.variables()] *= factor
        except KeyError:
            FR._add_factor(self,factor)
        

    def _add_factor_to_dict(self,hyperedge,factor):
        """Add a factor to the _factors dictionary

        @param hyperedge: The hyperedge for the factor
        @type hyperedge: Frozenset
        @param factor: The factor to be added
        @type factor: L{Factor}
        """
        self._factors[hyperedge] = factor

    _put_factor_to_dict = _add_factor_to_dict
    

    def copy(self,copy_domain=False):
        """Return a deep copy of a hierarchical model

        @param copy_domain: If true C{self}'s domain is copied, otherwise the copy
        shares C{self}'s domain
        @type copy_domain: Boolean
        @return: A copy of C{self}
        @rtype: Same type as C{self}
        """
        cp = SubDomain.copy(self,copy_domain)
        cp.__class__ = self.__class__
        cp._hypergraph = self._hypergraph.copy()
        factors = {}
        for hyperedge, factor in self._factors.items():
            fc = factor.copy()
            cp.common_domain(fc)
            factors[hyperedge] = fc
        cp._factors = factors
        return cp

    def factor(self,hyperedge):
        return self._factors[frozenset(hyperedge)]

    def factors(self):
        return self._factors.values()

    def factors_containing_variable(self,variable):
        return [self._factors[hyperedge] for
                hyperedge in self._hypergraph[variable]]

    def is_simple(self):
        return True

    def items(self):
        return self._factors.items()

    def simplify(self):
        pass

    def some_factor(self,hyperedge):
        return self._factors[frozenset(hyperedge)]

class GFR(FR):
    """Factored representations of probability distributions (associated hypergraph is graphical)

    Factors are defined exactly on the cliques of its interaction graph.
    If factors were (A,B), (A,C), (B,C) then would NOT be graphical.

    @todo: Actually implement methods specific to this class. At present L{GFR} objects
    behave identically to L{FR} objects so this class only useful as a label I{claiming}
    that a hierarchical model is graphical.
    """
    pass

class RFR(SFR):
    """Factored representations of probability distributions (associated hypergraph is reduced)

    A representation with no redundant factors.
    If factors were (A), (A,B) then would NOT be reduced.
    
    @todo: Actually implement methods specific to this class. At present L{RFR} objects
    behave identically to L{SFR} objects so this class only useful as a label I{claiming}
    that a hierarchical model is reduced.
    """

    def __init__(self,hm=(),check=True,modify=False):
        """Construct RFR from existing hierarchical model"""
        if not isinstance(hm,FR):
            hm = SFR(hm)
        if isinstance(hm,RFR):
            check = False
            modify = False
        rhg = ReducedHypergraph(hm._hypergraph,check,modify,trace=True)
        if modify:
            for redund, supersets in rhg.trace.items():
                superset = member(supersets)
                hm._factors[superset] *= hm._factors[redund]
                del hm._factors[redund]
            del rhg.trace
        if isinstance(hm,BN):
            del hm._adg
        self.__dict__ = hm.__dict__

    def _add_factor(self,factor):
        """Multiply a reduced factored representation by a factor

        If a factor containing all C{factor}'s variables exists then
        C{factor} is multiplied into it.

        @param factor: The factor to be multiplied in
        @type factor: L{Factor}
        @todo: implement properly
        """
        hyperedge = factor.variables()
        smallerhes = []
        for he in self._hypergraph:
            if not smallerhes and he >= hyperedge:
                self._factors[he] *= factor
                return
            elif he <= hyperedge:
                smallerhes.append(he)
        SFR._add_factor(self,factor)
        for he in smallerhes:
            self._factor[hyperedge] *= self._factor[he]
            self.remove(he)

    def ipf(self,marginals,epsilon=0.001):
        """Iterative proportional fitting

        Iteratively alters the marginals of C{self} until
        they (almost) equal those supplied by C{marginals}.
        The existing factors in C{self} act only as a starting
        point for the iterative algorithm.
        Iteration stops once there is no marginal probability in C{self}
        differing by more than C{epsilon} from the one supplied by C{marginals}.

        Each factor in C{self} must have exactly one corresponding marginal
        in C{marginals}.

        Typically C{marginals} will come from an empirical distribution, ie
        they will just be normalised counts from some data set.

        @param marginals: Marginals to fit
        @type marginals: Dictionary mapping hyperedges to marginals
        @param epsilon: Convergence criterion
        @type epslion: Float
        """
        converged = False
        while not converged:
            converged = True 
            for hyperedge, factor in self._factors.items():
                current_marginal = self.marginal_factor(hyperedge)
                desired_marginal = marginals[hyperedge]
                factor *= desired_marginal / current_marginal
                if converged and current_marginal.differ(desired_marginal,epsilon):
                    converged = False


        # jf = JFR(self.copy(),modify=True)
#         store = []
#         for old, new in jf.trace.items():
#             store.append((old, new, jf.perfect_sequence(new))
#         converged = False
#         while not converged:
#             converged = True # assumed converged until proven otherwise
#             for hyperedge, root, perfect_sequence in store:
#                 correction = (marginals[hyperedge] /
#                               jf.marginal(hyperedge,root,perfect_sequence))
#                 jf[root] *= correction
#                 if converged:
#                     for x in correction.data():
#                         if abs(x) > epsilon:
#                             converged = False
#                             break
#         for hyperedge, root, perfect_sequence in store:
#             self._factors[hyperedge] = jf[root].copy().marginalise_onto(hyperedge)


    
class DFR(GFR):
    """Factored representations of probability distributions (associated hypergraph is decomposable)
    
    Interaction graph is decomposable (triangulated)

    @note: There are no methods specific to this class. At present L{DFR} objects
    behave identically to L{FR} objects so this class is only useful as a label I{claiming}
    that a hierarchical model is decomposable. However there are methods for L{JFR} objects
    which are just (reduced) decomposable models with a particular join forest specified.
    """
    pass


class RDFR(RFR,DFR):
    """Factored representations of probability distributions (associated hypergraph is decomposable and reduced)
    
    Interaction graph is decomposable (triangulated)

    @note: There are no methods specific to this class. At present L{RDFR} objects
    behave identically to L{FR} objects so this class is only useful as a label I{claiming}
    that a hierarchical model is decomposable and reduced. However there are methods for L{JFR} objects
    which are just reduced decomposable models with a particular join forest specified.
    """
    pass


class JFR(RDFR):
    """Join forest representations of probability distributions

    A reduced, decomposable model where the factors are clique potentials and
    separators, related by a join forest.

    @ivar _hypergraph: The join forest associated with the model.
    @type _hypergraph: L{Hypergraphs.ReducedJoinForest}
    @ivar _factors: Maps each clique (node of the join_forest) to its associated factor
    @type _factors: Dictionary
    @ivar _separators: Maps each separator (edge of the join_forest) to its associated factor
    @type _separators: Dictionary
    """

    def __init__(self,hm=(),domain=None,new_domain_variables=None,
                 must_be_new=False,check=False,modify=False,elimination_order=None):
        """Construct a join forest model from an existing hierarchical model (or factors)

        If C{hm} is a hypergraph it will end up with identical attributes
        to C{self}, so typically a copy of an existing hypergraph is used.

        @param hm: Hierachical model or sequence of factors
        @type hm: L{FR} or sequence
        @param domain: B{Only used if C{hm} is not an L{FR} object.} A domain for the model.
        If None the internal default domain is used.
        @type domain: L{Variables.Domain} or None
        @param new_domain_variables: B{Only used if C{hm} is not an L{FR} object.} A dictionary
        containing a mapping from any new
        variables to their values. C{domain} is updated with these values
        @type new_domain_variables: Dict or None
        @param must_be_new: B{Only used if C{hm} is not an L{FR} object.} Whether domain
        variables in C{new_domain_variables} have to be new
        @type must_be_new: Boolean
        @param check: B{Only used if C{hm} is not an L{FR} object.} Whether to check
        that all variables exist in C{domain}
        @type check: Boolean
        @param modify: Whether to modify C{hm} to make it decomposable.
        @type modify: Boolean
        @param elimination_order: If supplied
        and C{modify=True}, the elimination order to use to make the C{hm}
        decomposable. (If not supplied maximum cardinality search is used to generate an order.)
        @type elimination_order: Sequence
        @raise VariableError: If a variable in C{new_domain_variables}
        already exists with values different from
        its values in C{new_domain_variables};
        Or if C{must_be_new} is set and the variable already exists.
        Or if C{check} is set and a variable in C{variables} is not in the domain
        @raise DecomposabilityError: If C{modify=False} and C{hm} is not decomposable.
        """
        if not isinstance(hm,FR):
            hm = FR(hm,domain,new_domain_variables,must_be_new,check)
        join_forest = ReducedJoinForest(hm._hypergraph,modify,True,elimination_order)
        if modify:
            fs = {}
            for hyperedge in join_forest:
                fs[hyperedge] = Factor(hyperedge,domain=hm)
            for old_hyperedge, hyperedge in join_forest.trace.items():
                fs[hyperedge] *= hm._factors[old_hyperedge]
            hm._factors = fs
            del join_forest.trace
        seps = {}
        for clique1, clique2 in join_forest._uforest.lines():
            seps[frozenset([clique1,clique2])] = Factor(clique1 & clique2,
                                                        domain=hm)
        hm._hypergraph = join_forest
        hm._separators = seps
        self.__dict__ = hm.__dict__

    def __str__(self):
        return 'Cliques:\n%sSeparators:\n%sJoin Forest:\n%s' % (
            RFR.__str__(self),
            FR(self._separators.values()),
            self._hypergraph)

    def calibrate(self):
        """Alter a JFR so that the factors associated with both cliques and
        separators are the appropriate marginal distributions
        """
        perfect_sequence = self._hypergraph.perfect_sequence()
        self.send_messages(perfect_sequence[:])
        perfect_sequence.reverse()
        self.send_messages(perfect_sequence)

    def condition(self,condition):
        """Alter a JFR by effecting the restriction on
        variables given by C{condition}

        This alters the model's domain. Make a copy with C{copy_domain=True}
        if the original domain will be needed
        @param condition: Dictionary of the form {var1:value1,var2:value2..}
        @type condition: Dict
        """
        self._check_condition(condition)
        # change data in each factor
        for factor in self._factors.values():
            factor.data_restrict(condition)
        # change data in each separator
        for sep in self._separators.values():
            sep.data_restrict(condition)
        # change stored values
        self.change_domain_variables(condition)
        return self

    def copy(self,copy_domain=False):
        """Return a deep copy of a JFR

        @param copy_domain: If true C{self}'s domain is copied, otherwise the copy
        shares C{self}'s domain
        @type copy_domain: Boolean
        @return: A copy of C{self}
        @rtype: L{JFR}
        """
        cp = FR.copy(self,copy_domain)
        seps = {}
        # bug corrected by Jon Ronson Tue Dec 12 17:40:33 GMT 2006
        for hyperedge, sep in self._separators.items():
            sc = sep.copy()
            cp.common_domain(sc)
            seps[hyperedge] = sc
        cp._separators = seps
        return cp

    def gui_calibrate(self,parent):
        clique_win = Tkinter.Frame(parent)
        clique_win.pack()
        self._clique_disp(clique_win,self._factors)
        sep_win = Tkinter.Frame(parent)
        sep_win.pack()
        self._clique_disp(sep_win,self._separators)
        gc =  GraphCanvas(self._hypergraph._uforest,parent,edit=False,pp_vertex=pretty_str_set,
                         colour_user_actions=False)
        gc.pack()
        bottom_win = Tkinter.Frame(parent)
        bottom_win.pack()
        perfect_sequence = self._hypergraph.perfect_sequence()
        cpps = perfect_sequence[:]
        banned = set()
        updone=[False]
        def handler(self=self,cliques=cpps,banned=banned,clique_win=clique_win,
                    sep_win=sep_win,updone=updone,gc=gc):
            if not cliques:
                if updone[0]:
                    print 'All messages sent and received'
                    return
                cliques[:] = perfect_sequence
                cliques.reverse()
                banned.clear()
                updone[0] = True
                print 'ROOT has received all messages'
            clique = cliques.pop()
            for nbr in self._hypergraph.clique_neighbours(clique,banned):
                self.send_message(clique,nbr)
                print 'Sent from %s to %s' % (clique,nbr)
                for clique2 in self._hypergraph:
                    gc.vertex_config(clique2,fill='black')
                gc.vertex_config(clique,fill='blue')
                gc.vertex_config(nbr,fill='red')
            banned.add(clique)
            self._clique_disp(clique_win,self._factors)
            self._clique_disp(sep_win,self._separators)
        Tkinter.Button(bottom_win,text='Next',command=handler).pack()

    def marginal(self,variables,root=None,perfect_sequence=None):
        """Compute the marginal distribution of C{variables} B{assuming they
        are a subset of some hyperedge (ie clique)}

        Just chooses an appropriate hyperedge as a root and sends messages to it.
        Typically used just after C{self} has been altered so that, at least,
        this root clique has the correct marginal.

        Used in iterative proportional fitting.
        
        @param variables: The variables for which a marginal is sought.
        @type variables: Iterable
        @param root: If supplied, a clique which is simply B{assumed} to contain
        the variables. If not supplied one is computed.
        @type root: Frozenset
        @param perfect_sequence: If supplied an ordering B{assumed} to be a perfect
        sequence with C{root} as its first element.
        @type perfect_sequence: Sequence
        @raise ValueError: If no clique contains C{variables}
        """
        variables=frozenset(variables)
        if root is None:
            for v in variables:
                try:
                    containers &= self._hypergraph.star(v)
                except NameError:
                    containers = self._hypergraph.star(v)
                if not containers:
                    raise ValueError("No clique contains %s")
            root = member(containers)
        if perfect_sequence is None:
            perfect_sequence = self.perfect_sequence(root)
        self.send_messages(perfect_sequence)
        return self._factors[root].copy().marginalise_onto(variables)
            
    def send_message(self,frm,to):
        """Send a message from a clique to a neighbouring clique

        @param frm: The clique sending the message
        @type frm: Frozenset (hyperedge)
        @param to: The clique receiving the message
        @type to:  Frozenset (hyperedge)
        @raise KeyError: If C{frm} and C{to} are not neighbours in the join
        forest
        """
        frm_marginal = self._factors[frm].sumout(frm - to)
        edge = frozenset([frm,to])
        self._factors[to] *= (frm_marginal/self._separators[edge])
        self._separators[edge] = frm_marginal

    def send_messages(self,cliques,banned=None):
        """Send messages using a fixed ordering of cliques

        C{cliques} acts as a I{stack}. The first clique which is allowed
        to send a message is the I{last} element of the list C{cliques}.
        Thus the root is the first element.
        (This is just a little quicker than starting with the first.)
        A clique can only send a message to a clique 'underneath' it in the stack.

        No message to a clique in C{banned} is allowed to be sent.
        @param cliques: Stack of cliques
        @type cliques: List
        @param banned: Cliques which are banned from receiving messages
        @type banned: set
        """
        if banned is None:
            banned = set()
        if cliques:
            clique = cliques.pop()
            for nbr in self._hypergraph.clique_neighbours(clique,banned):
                self.send_message(clique,nbr)
            banned.add(clique)
            self.send_messages(cliques,banned)

    def separator_items(self):
        """Return (separator,separator_factor) pairs for all separators

        The order is arbitrary.
        
        @return: (separator,separator_factor) pairs for all separators
        @rtype: List
        """
        return self._separators.items()

        
    def separator_factors(self):
        """Return an iterator over the factors associated with separators in the model

        The order is arbitrary.
        @return: An iterator over the factors associated with separators in the model
        @rtype: Iterator
        """
        return self._separators.itervalues()

    def trace(self):
        return self._hypergraph.trace

    def var_marginal(self,variable):
        """Compute the marginal distribution for C{variable},
        assuming that C{self} is calibrated
        """
        l = len(self._variables)
        for factor in self.factors_containing_variable(variable):
            this_l = len(factor.variables())
            if this_l <= l:
                smallest_factor = factor
                l = this_l
        return CPT(smallest_factor.copy().marginalise_onto([variable]).normalised(),variable)

    def _clique_disp(self,clique_win,factor_dict):
        for child in clique_win.winfo_children():
            child.destroy()
        fgs = []
        for hyperedge, factor in factor_dict.items():
            fgs.append(factor.gui_main(clique_win,edit=False))
            fgs.append(Tkinter.Label(clique_win,text='*'))
        fgs.pop()
        for widget in fgs:
            widget.pack(side=Tkinter.LEFT)


class BN(SFR):
    """Bayesian network representations of probability distributions

    A SFR where the factors consist of a CPT
    for each variable.

    Joint is thus exactly the product of the factors
    @ivar _adg: The acyclic digraph giving the BN's structure
    @type _adg: L{ADG} object
    """

    def __init__(self,factors=(),domain=None,new_domain_variables=None,
                 must_be_new=False,check=False,adg=None):
        """BN constructor.
        Each factor has its domain expanded to be the domain of the model, if necessary.

        If C{factors} is a sequence of factors then each is merely
        B{assumed} to be a CPT object, but there is no check done.
        
        @param factors: The factors in the hierarchical model.
        (Alternatively an existing object of class L{FR} (or its subclasses), in
        which case C{self} has identical attributes, except possibly the attribute specifying its
        C{adg}.)
        @type factors: Sequence, each element of which is a L{Parameters.Factor} or
        L{Variables.SubDomain} object. (Alternatively an object of class L{FR}
        or one of its subclasses.)
        @param domain: A domain for the model.
        If None the internal default domain is used.
        @type domain: L{Variables.Domain} or None
        @param new_domain_variables: A dictionary containing a mapping from any new
        variables to their values. C{domain} is updated with these values
        @type new_domain_variables: Dict or None
        @param must_be_new: Whether domain variables in C{new_domain_variables} have
        to be new
        @type must_be_new: Boolean
        @param check: Whether to check that all variables exist in C{domain}
        @type check: Boolean
        @param adg: The acyclic digraph for the BN. This is simply B{assumed} to
        be the correct ADG, no check is made. If C{adg} is not supplied the correct ADG
        is created if possible, otherwise a C{DirectedCycleError} is raised.
        @type adg: L{Graphs.ADG}
        @raise VariableError: If a variable in C{new_domain_variables}
        already exists with values different from
        its values in C{new_domain_variables};
        Or if C{must_be_new} is set and the variable already exists.
        Or if C{check} is set and a variable in C{variables} is not in the domain
        @raise AttributeError: If C{adg} was not supplied and any of the supplied
        factors are not L{Parameters.CPT} objects. 
        @raise DirectedCycleError: If C{adg} was not supplied and there is no ADG
        consistent with the CPTs provided.
        """
        SFR.__init__(self,factors,domain,new_domain_variables,must_be_new,check)
        if adg is None:
            adg = ADG()
            for cpt in self:
                adg.put_family(cpt.child(),cpt.parents())
        self._adg = adg

    def __getitem__(self,key):
        """Return the CPT (not a copy) corresponding to C{key}

        C{key} can be all variables in
        CPT or just the child variable. The former is quicker.
        @param key: Family or child variable
        @type key: Iterable
        @return: The CPT for var
        @rtype: L{CPT}
        @raise KeyError: If there is no corresponding CPT.
        """
        if isinstance(key,str):
            return self._factors[frozenset([key]) | self._adg.parents(key)]
        else:
            return SFR.__getitem__(self,key)

    def __repr__(self):
        """Formal string representation of a BN

        @return: Formal string representation of a BN
        @rtype: String
        """
        cpts = ','.join([cpt.repr_nodomain() for cpt in self._factors.values()])
        dkt = dict([(v,self._domain[v]) for v in self._variables])
        return 'BN([%s],Domain(new_domain_variables=%s),adg=%s)' % (cpts,dkt,repr(self._adg))

    def __str__(self):
        """Print each CPT separately

        Use lexicographical order on child name

        @return: Pretty representation of a L{BN}
        @rtype: String
        """
        out = ''
        tmp = []
        for cpt in self._factors.values():
            tmp.append((cpt.child(),cpt))
        tmp.sort()
        for l, cpt in tmp:
            out += str(cpt)
            out += '\n'
        return out


    def _add_factor(self,factor):
        """Include a factor in a BN

        If the factor is a CPT, its child is a new variable and all
        its parents are existing variables, then C{self}
        remains a BN, otherwise it becomes a L{FR} object
        """
        if (isinstance(factor,CPT) and
            factor.child() not in self._variables and
            factor.parents() <= self._variables):
            self._adg.put_family(factor.child(),factor.parents())
        else:
            self.__class__ = FR
            del self._adg
        FR._add_factor(self,factor)

    def adg(self):
        """Return the ADG associated with BN

        A copy of the BN's ADG is returned.
        @return: The ADG associated with BN
        @rtype: L{Graphs.ADG}
        """
        return self._adg.copy()

    def add_cpts(self,cpts):
        """Adds CPTs to a BN, if possible

        The list C{cpts} is destroyed by this process

        @param cpts: CPTs to add
        @type cpts: list
        @raise AttributeError: If any of the C{cpts} is not a L{Parameters.CPT} object
        @raise ValueError: If by adding these CPTs C{self} would no longer be a BN.
        """
        while cpts:
            for i, cpt in enumerate(cpts):
                if (cpt.child() not in self._variables
                    and cpt.parents() <= self._variables):
                    self *= cpt
                    del cpts[i]
                    break
            else:
                raise ValueError('Could not add CPTs: %s' % cpts)

    def bdeu_score(self,data,precision=1.0):
        """
        Return the BDeu score of C{self} (and its component scores) on C{data}.

        @param data: The data
        @type data: L{Parameters.CompactFactor}
        @param precision: Prior precision
        @type precision: Float
        @return: C{(score,variable_scores)} where C{score} is the BDeu score and
        C{variable_scores} is a list of component scores, one for each variable (in order)
        in C{self}.
        @rtype: Tuple
        @raise NameError: If L{gPyC} was not successfully imported
        """
        variable_scores = []
        score = 0.0
        for variable in self.variables():
            new_score = self[variable].get_counts(data).bdeu_score(precision)
            score += new_score
            variable_scores.append(new_score)
        return score, variable_scores


    def bdeu_score_using_other(self,data,other,other_score,other_variable_scores,precision=1.0):
        """
        Return the BDeu score of C{self} (and its component scores) on C{data}
        using the existing BDeu score (and component scores) for C{other}
        computed on C{data} previously.

        Clearly, for this to give the right answer, the existing score must have used the same C{precision}.

        @param data: The data
        @type data: L{Parameters.CompactFactor}
        @param other: Previously scored BN
        @type other: L{BN}
        @param other_score: The BDeu score of C{other} on C{data}
        @type other_score: Float
        @param other_variable_scores: For each variable (in order) in C{other}, the
        component of the BDeu score for that variable
        @type other_variable_scores: List
        @param precision: Prior precision
        @type precision: Float
        @return: C{(score,variable_scores)} where C{score} is the BDeu score and
        C{variable_scores} is a list of component scores, one for each variable in
        C{self}.
        @rtype: Tuple
        @raise NameError: If L{gPyC} was not successfully imported
        """
        variable_scores = other_variable_scores
        score = other_score
        for i, variable in enumerate(self.variables()):
            if self.adg.parents(variable) != other.adg.parents(variable):
                new_score = self[variable].get_counts(data).bdeu_score(precision=1.0)
                score += (new_score - other_variable_scores[i])
                variable_scores[i] = new_score
        return score, variable_scores

    def children(self,variable):
        """Return the children of C{variable}

        @param variable: Variable in the BN
        @type variable: Immutable (usually a string)
        @return: The children of C{variable}
        @rtype: set
        @raise KeyError: If C{variable} is not in the BN.
        """
        return self._adg.children(variable)

    def condition(self,condition,keep_class=False):
        """Alter a Bayesian network by effecting the restriction on
        variables given by C{condition}

        Generally conditioning a BN produces something which is not
        a BN, see C{keep_class} argument.

        This alters the model's domain. Make a copy with C{copy_domain=True}
        if the original domain will be needed
        @param condition: Dictionary of the form {var1:values1,var2:values2..}
        Each value of this dictionary must be an iterable
        @type condition: Dict
        @param keep_class: If C{False}, the object will cease to be a
        L{BN} object, becoming a {SFR} object, and all its factors will become
        L{Parameters.Factor} object. If C{True} remains a BN.
        @type keep_class: Boolean
        @return: The conditioned model
        @rtype: Same as C{self}
        @raise KeyError: If a variable is used that is not in the model
        @raise ValueError: If a value is used that is not a possible value of
        the variable it is attached to
        """

        SFR.condition(self,condition,keep_class)
        if not keep_class:
            self.__class__ = SFR
        return self
        
    def copy(self,copy_domain=False):
        """
        Return a copy

        @return: A copy of C{self}
        @rtype: L{BN}
        """
        cp = SFR.copy(self,copy_domain)
        cp._adg = self._adg.copy()
        return cp

    def eliminate_variable(self,variable,trace=False):
        """Alter a Bayesian network by summing out a variable

        C{self} will cease to be a L{BN} object unless C{variable}
        has no children.
        @param variable: The variable to eliminate
        @type variable: String
        """
        if not self._adg.children(variable):
            if trace:
                return SFR.eliminate_variable(self,variable,True)
            self.remove(variable)
        else:
            self.__class__ = SFR
            return self.eliminate_variable(variable,trace)

    def family_hyperedge(self,child):
        """Return the hyperedge corresponding to the CPT with C{child}
        as child

        @param child: Variable
        @type child: Immutable
        @return: Hyperedge corresponding to the CPT with C{child}
        as child
        @rtype: Frozenset
        """
        return frozenset([child]) | self._adg.parents(child) 

    def from_bif(self,info):
        """Add CPTs from a C{.bif} file

        @param info: Output from L{IO.read_bif}
        @type info: Tuple

        """
        varvalues, parents, cpts = info
        self.add_domain_variables(varvalues)
        new_cpts = []
        for var, parents in parents.items():
            cpt = CPT(Factor(parents+(var,),domain=self),child=var)
            for inst, probs in cpts[var].items():
                dkt = dict(zip(parents,inst))
                for i, pr in enumerate(probs):
                    dkt[var] = varvalues[var][i]
                    cpt[dkt] = pr
            new_cpts.append(cpt)
        self.add_cpts(new_cpts)

    def from_dnet(self,info):
        """Add CPTs from a C{.dnet} file

        @param info: Output from L{IO.read_dnet}
        @type info: Tuple

        """
        try:
            dnet_variables, named_cpts = info
        except ValueError:
            dnet_variables, named_cpts, coords = info
        cpts = []
        for variable, (parents,data) in named_cpts.items():
            cpts.append(CPT(
                Factor(parents+[variable],data,self,
                       new_domain_variables=dnet_variables,
                       check=True),variable,cpt_check=True))
        self.add_cpts(cpts)
        try:
            self._adg.set_vertex_positions(coords)
        except NameError:
            pass


    def parents(self,variable):
        """Return the parents of C{variable}

        @param variable: Variable in the BN
        @type variable: Immutable (usually a string)
        @return: The parents of C{variable}
        @rtype: set
        @raise KeyError: If C{variable} is not in the BN.
        """
        return self._adg.parents(variable)

    def remove(self,key):
        """Remove a CPT where C{key} is either the child or a tuple giving
        the family for the CPT.

        Using the child as key is faster.
        @raise TypeError: If C{key} is neither a variable or a family
        """
        key_hyperedge = frozenset(key)
        if key_hyperedge in self._factors:
            hyperedge = key_hyperedge
            for child in hyperedge:
                parents = self._adg.parents(child)
                if hyperedge == parents | set([child]):
                    break
        else:
            child = key
            hyperedge = self.family_hyperedge(child)
        if not self._adg.children(child):
            self._adg.remove_vertex(child)
        else:
            # can do this since a SFR has same _factors dictionary
            self.__class__ = SFR
            del self._adg
        SFR.remove(self,hyperedge)


    def sample(self,fobj,samples=100):
        """Write a sample in CSV form to C{fobj}

        Uses forward sampling, so assumes no instantiations

        @param fobj: Writable file
        @type fobj: File
        @param samples: How may samples to produce
        @type samples: Int
        """
        from gPy.Samplers import BNSampler
        for v in self._variables:
            print >>fobj, '%s:%s' % (v, ','.join(self._domain[v]))
        sampler = BNSampler(self)
        print >>fobj, ','.join(sampler.variables())
        while samples:
            print >>fobj, ','.join(sampler.forward_sample())
            samples -= 1

    def sample_sqlite(self,dbfilename=':memory',table='data',samples=100):
        """Write a sample to a sqlite database

        Uses forward sampling, so assumes no instantiations

        @param db: Name of sqlite database
        @type fobj: String
        @param samples: How may samples to produce
        @type samples: Int
        @return: sqlite database
        @rtype: L{Parameters.SqliteFactor) object
        """
        from gPy.Samplers import BNSampler
        from gPy.Parameters import SqliteFactor
        sqlfactor = SqliteFactor(self.variables(),dbfilename,table,domain=self)
        newself = self.copy(True) # deep copy
        dom = newself._domain
        # replace values with numbers
        for v, vals in dom.items():
            dom[v] = range(len(vals))
        sampler = BNSampler(newself)
        dkt = {}
        while samples:
            inst = tuple(sampler.forward_sample())
            try:
                dkt[inst] += 1
            except KeyError:
                dkt[inst] = 1
        sqlfactor.populate([inst+(count,) for inst,count in dkt.items()],
                           sampler.variables())
        return sqlfactor


    def topological_order(self):
        """A topological ordering of the associated acyclic digraph

        Children come after their parents in the ordering
        @return: A topological ordering of the vertices
        @rtype: List
        """
        return self._adg.topological_order()

#     def sample(self,times=1):
#         """UNIMPLEMENTED

#         Simple rejection sampling from the (perhaps conditional) joint
#         Generated sample may be smaller than times due to rejections.
#         """
#         sample = []
#         count = 0
#         save_instantiation = self.copy_instantiation()
#         while count < times:
#             inst_list = []
#             for v in self.topological_order:
#                 value = self[v].sample_cpt()
#                 if value == None:
#                     break
#                 self.update_instantiation({v:[value]})
#                 inst_list.append(value)
#             else:
#                 sample.append(tuple(inst_list))
#             count += 1
#             self.update_instantiation(save_instantiation)
#         return sample

#     def _gettopological_order(self):
#         try:
#             return self._topological_order
#         except AttributeError:
#             self._topological_order = self.adg.topological_order()
#             return self._topological_order
#     def _deltopological_order(self): del self._topological_order
#     _doctopoligical_order = """
#     A topological ordering of the associated acyclic digraph

#     Computed on demand and then cached.
#     """
#     topological_order = property(fget=_gettopological_order,
#                                  fdel=_deltopological_order,
#                                  doc=_doctopoligical_order)
        
#         self._hypergraph = dm._hypergraph
#         self._factors = dm._factors
#         join_forest = dm.hypergraph().join_forest()
#         TODO!!
#         fs = {}
#         seps = {}
#         for hyperedge in join_forest:
#             fs[hyperedge] = Factor(hyperedge)
#         for edge in join_forest.edges:
#             clique1, clique2 = tuple(edge)
#             seps[edge] = Factor(clique1 & clique2)
#         destination = join_forest.destination
#         for hyperedge, factor in hm.items():
#             fs[destination[hyperedge]] *= factor
#         self._factors = fs
#         self._hypergraph = join_forest
#         self._separators = seps



class CBN(BN):
    def __init__(self,factors=(),domain=None,adg=None):
        super(CBN,self).__init__(factors=factors,domain=domain,adg=adg)

    @staticmethod
    def from_bn(bn):
        """Construct a L{CBN} from a L{BN}."""
        self = BN()
        self.__dict__ = bn.__dict__
        self.__class__ = CBN
        self._adg = MutilatedADG.from_adg(self._adg)
        return self

    @staticmethod
    def from_adg_data(adg, data, prior=1):
        """Construct a L{CBN} from an L{ADG} and estimates of the parameters from
        some observations.
        @type adg: L{ADG}
        @type data: L{CausalWorld}
        @param prior: the Dirichlet prior parameter (the same parameter value
        is used for all instances!)  Note there may be some problems with
        this method: a B{different} prior is used by the BDeu score. However,
        in practice, for parameter estimation, this prior method seems to be ok.
        I was lazy and it was simple to implement (cb).  If prior is zero, then
        the parameters are the maximum likelihood estimation solutions.
        """
        cpts = []
        for child in data.variables():
            cpts.append(data.makeCPT(child,adg.parents(child),force_cpt=True, prior=prior))
        self = CBN(factors=cpts, domain=data, adg=adg.copy())
        return self

    def _mutilate(self, variables):
        for variable in variables:
            child = frozenset([variable])
            n = self._numvals[variable]
            dat = [1/float(n) for i in xrange(n)]
            self._replace_factor( variable
                               , Factor(variables=child,data=dat,domain=self).makeCPT(variable)
                               , allow_hyperedge_change=True)
            parents = self._adg.parents(variable)
            self._hypergraph.remove_hyperedge(child | parents)
            for parent in parents:
                self._adg.remove_arrow(parent, variable)
            self._hypergraph.add_hyperedge(child)

    def intervene(self, intervention):
        """
        @param intervention: A dictionary mapping variables in the L{CBN} to
        a single value in the domain.
        """
        self._mutilate(intervention.keys())
        self.condition(intervention, keep_class=True)

    def _replace_factor(self, child, factor, allow_hyperedge_change=False):
        """Replace the factor of C{child} with C{factor}, updating the
        hypergraph. Note the ADG must be update separately!"""
        hyperedge = self.family_hyperedge(child)
        new_hyperedge = frozenset(factor.variables())
        if new_hyperedge != hyperedge and not allow_hyperedge_change:
            raise ValueError,'new factor does not have same variables as hyperedge',hyperedge
        del self._factors[hyperedge]
        self._factors[new_hyperedge] = factor

    def estimate_parameters(self, data, prior=1.0):
        """Replace the parameters of C{self} with estimates from C{data}, keeping the
        same structure.
        @param prior: the Dirichlet prior parameter (the same parameter value
        is used for all instances!)  Note there may be some problems with
        this method: a B{different} prior is used by the BDeu score. However,
        in practice, for parameter estimation, this prior method seems to be ok.
        I was lazy and it was simple to implement (cb).  If prior is zero, then
        the parameters are the maximum likelihood estimation solutions.
        """
        for child in self._variables:
            self._replace_factor(child, data.makeCPT(child,self._adg.parents(child),force_cpt=True, prior=prior))

    def extend_to(self, other):
        """Extend the domains of each of the variables in C{other}
        to the corresponding values. The parameters corresponding to values
        introduced to the domains are set to zero. When conditioning, L{BN}
        removes elements from the domains of variables. Sometimes it is useful to
        add the zeroes e.g. when using an additive binary operator.
        @param other: a dictionary mapping variables to their domains. Note
        the current domain must be a subset of the new domain.
        """
        for factor in self:
            extension = dict([(variable, other.values(variable))
                    for variable in other.variables() & factor.variables()])
            factor.data_extend(extension, keep_class=True)

        # factors should have the same domain as self
        for variable in other.variables():
            self.change_domain_variable(variable, other.values(variable))

    def good_interventions(self):
        """Obtain a heuristically ``good'' set of interventions from which
        structures can be learnt."""
        forces_vars = self.good_sets_forced_variables()
        # intervene at these variables, trying every combination of value
        interventions = []
        for forced_vars in forces_vars:
            forced_vars = tuple(forced_vars)
            for inst in self.insts(forced_vars):
                inst = map(lambda x: frozenset([x]), inst)
                interventions.append(dict(zip(forced_vars,inst)))
        return interventions

    def good_sets_forced_variables(self):
        """Return the good independent sets of variables to force.  This is an
        approximation since ideally you want the edge which implies the most
        propagation when resolved. The approximation will resolve all edges.
        However some forcing may be unnecessary."""
        def fanout_cmp(a,b):
            return cmp(fanout[b], fanout[a])
        interventions = []
        g = EssentialGraph.from_graph(self._adg.essential_graph())
        while g.vertices():
            # propagate any implied orientations
            g.resolve()

            # set of vertices adjacent to force variables
            blanket = set()

            # map from vertex to fanout
            fanout = {}

            # add another set of forced variables to the list
            interventions.append(set())

            # determine the fan-out of each node
            for v in g.vertices():
                f = len(g.neighbours(v))
                if f < 1:
                    g.remove_vertex(v)
                    continue
                fanout[v] = f

            # order nodes by their fan-out
            fanout_order = sorted(fanout.keys(), cmp=fanout_cmp)

            # intervene at the highest fan-out nodes first
            for v in fanout_order:
                # ... provided they are not adjacent to previous force
                # variables
                if v in blanket:
                    continue
                interventions[-1].add(v)
                # update the adjacent nodes
                blanket |= g.neighbours(v)
                # remove this node so that it is not included in the next
                # intervention fan-out calculations
                g.remove_vertex(v)
        return interventions

