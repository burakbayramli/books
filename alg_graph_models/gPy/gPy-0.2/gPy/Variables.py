"""For manipulation of (random) variables

@var _version: Version of this module
@type _version: String
"""

_version = '$Id: Variables.py,v 1.3 2008/10/07 09:14:46 jc Exp $'

class VariableError(Exception):
    """Base class for errors in the Variables module"""
    pass

import operator

def extdiv(x,y):
    """Return x/y where 0/0 = 0

    @param x: LHS of division
    @type x: Numeric
    @param y: LHS of division
    @type y: Numeric
    @return: C{x/y} where 0/0 = 0
    @rtype: Numeric
    @raise ZeroDivisionError: If C{y==0} and C{x!=0}.
    """
    try:
        return x / y
    except ZeroDivisionError:
        if x == 0: return 0
        else: raise ZeroDivisionError

class Domain(object):
    """A domain is a set of random variables, implicitly representing all functions
    from joint instantiations of the variables

    @ivar _domain: A dictionary mapping each variable to its set of possible values.
    Each key is a variable name. Each value is a frozenset.
    @type _domain: Dictionary
    @ivar _numvals: A dictionary mapping each variable to the number of values it can have.
    Each key is a variable name. Each value is a positive integer
    @type _numvals: Dictionary
    @ivar _instd: Set of instantiated variables
    @type _instd: Set
    """

    def __init__(self,domain=None,new_domain_variables=None,must_be_new=False):
        """Construct a domain

        @param domain: An existing domain to use. If None the object will be
        independent of any existing domains. Otherwise C{self} and C{domain} will have identical
        attributes.
        @type domain: L{Domain} or None
        @param new_domain_variables: A dictionary containing a mapping from any new
        variables to their values.
        @type new_domain_variables: Dict or None
        @param must_be_new: Whether domain variables in C{new_domain_variables} have
        to be new
        @type must_be_new: Boolean
        @raise VariableError: If a variable in C{new_domain_variables}
        already exists with values different from
        its values in C{new_domain_variables};
        Or if C{must_be_new} is set and the variable already exists.
        """
        if domain is None:
            self._domain = {}
            self._numvals = {}
            self._instd = set()
        else:
            self._domain = domain._domain
            self._numvals = domain._numvals
            self._instd = domain._instd
        if new_domain_variables:
            self.add_domain_variables(new_domain_variables,must_be_new)

    def __repr__(self):
        return 'Domain(None,%s)' % self._domain

    def __str__(self):
        return str(self._domain)

    def add_domain_variable(self,variable,values,must_be_new=False):
        """Add a variable and its associated values 
    
        If C{variable} already exists then a check is done to ensure that C{values}
        is correct.
        @param variable: The new variable
        @type variable: Immutable
        @param values: The values of the new variable
        @type values: Iterable
        @param must_be_new: If the variable should be a new variable
        @type must_be_new: Boolean
        @raise VariableError: If C{variable} already exists with values different from
        C{values}; Or if C{must_be_new} is set and the variable already exists.
        """
        values = frozenset(values)
        if variable in self._domain:
            if must_be_new:
                raise VariableError("%s already exists" % variable)
            if values != self._domain[variable]:
                raise VariableError("Conflicting values for %s\n new: %s\n old: %s" %
                                    (variable, values, self._domain[variable]))
        else:
            self._domain[variable] = values
            lvals = len(values)
            self._numvals[variable] = lvals
            if lvals == 1:
                self._instd.add(variable)

    def add_domain_variables(self,new_domain_variables,must_be_new=False):
        """Add variables from C{new_domain_variables}

        @param new_domain_variables: A dictionary mapping variables to values
        @type new_domain_variables: Dictionary
        @param must_be_new: Whether domain variables in C{new_domain_variables} have
        to be new
        @type must_be_new: Boolean
        @raise VariableError: If a variable in C{new_domain_variables}
        already exists with values different from
        its values in C{new_domain_variables};
        Or if C{must_be_new} is set and the variable already exists.
        """
        for variable, values in new_domain_variables.items():
            self.add_domain_variable(variable,values,must_be_new)

    def add_domain_variables_from_rawdata(self,rawdata,must_be_new=False):
        """Add variables from C{rawdata}

        @param rawdata: A tuple like that returned by L{IO.read_csv}.
        @type rawdata: Tuple
        @param must_be_new: Whether domain variables from C{rawdata} have
        to be new
        @type must_be_new: Boolean
        @raise VariableError: If a variable in C{rawdata}
        already exists with values different from
        its values in C{rawdata};
        Or if C{must_be_new} is set and the variable already exists.
        """
        self.add_domain_variables(rawdata[1],must_be_new)

    def change_domain_variable(self,variable,values):
        """Change the values associated with a domain variable 
    
        @param variable: The variable
        @type variable: Immutable
        @param values: The new values of the C{variable}
        @type values: Iterable
        @raise KeyError: If C{variable} is not in the domain
        """
        values = frozenset(values)
        self._domain[variable] = values
        lvals = len(values)
        self._numvals[variable] = lvals
        if lvals == 1:
            self._instd.add(variable)
        else:
            self._instd.discard(variable)
            
    def change_domain_variables(self,new_values):
        """Change the values associated with a domain variables 
    
        @param new_values: The new values of the C{variable}
        @type new_values: Iterable
        @raise KeyError: If C{new_values} contains a variable not in the domain
        """
        for variable, values in new_values.items():
            self.change_domain_variable(variable,values)

    def common_domain(self,other):
        """Make the domain for C{other} identical to that of C{self}

        The domain for self is updated to include any extra variables in C{other}
        @param other: Domain
        @type other: L{Domain} or subclass
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        if self._domain is not other._domain:
            self.add_domain_variables(other._domain)
            other._domain = self._domain
            other._numvals = self._numvals
            other._instd = self._instd
            
    def copy(self):
        """Return a (deep) copy of a domain

        @return: The copy
        @rtype: L{Domain}
        """
        return Domain(new_domain_variables=self._domain)

    def known_variable(self,variable):
        """Whether C{self} knows C{variable} (and thus its vales)

        If, as is common, C{self} uses the internal default domain to
        keep track of variables, this amounts to checking whether the
        variable is in the internal default domain.

        @param variable: A variable
        @type variable: Immutable (usually string)
        """
        return variable in self._domain
        
    def numvals(self,variable):
        """Return the number of values associated with a variable

        @param variable: Variable whose number of values is sought.
        @type variable: String
        @return: The number of values associated with a variable
        @rtype: Int
        @raise KeyError: If C{variable} is not in the domain
        """
        return self._numvals[variable]
            
    def values(self,variable):
        """Return the values associated with a variable

        @param variable: Variable whose number of values is sought.
        @type variable: String
        @return: The values associated with a variable
        @rtype: Frozenset
        @raise KeyError: If C{variable} is not in the domain
        """
        return self._domain[variable]

_default_domain = Domain()
"""Default domain

Initially empty. IGNORE any value given in epydoc documentation. If there
is one this is just an unwanted by product of the way the documentation is produced.
"""

def clear_default_domain():
    """Reset the internal default domain to be empty"""
    global _default_domain
    _default_domain = Domain()

def print_default_domain():
    """Print out the internal default domain
    
    """
    print _default_domain

def set_default_domain(dict):
    """Set the internal default domain
    
    Any previous values will be deleted!
    
    @param dict: Mapping from each variable to the values it can have
    @type dict: Dictionary
    """
    global _default_domain
    _default_domain = Domain(new_domain_variables=dict)

def declare_variable(variable,values,must_be_new=False):
    """Add a variable and its associated values to the default domain 
    
    If C{variable} already exists then a check is done to ensure that C{values}
    is correct.
    @param variable: The new variable
    @type variable: Immutable
    @param values: The values of the new variable
    @type values: Iterable
    @param must_be_new: If the variable should be a new variable
    @type must_be_new: Boolean
    @raise VariableError: If C{variable} already exists with values different from
    C{values}; Or if C{must_be_new} is set and the variable already exists.
    """
    _default_domain.add_domain_variable(variable,values,must_be_new=False)

def change_variable(self,variable,values):
    """Change the values associated with a domain variable 
    
    @param variable: The variable
    @type variable: Immutable
    @param values: The new values of the C{variable}
    @type values: Iterable
    @raise KeyError: If C{variable} is not in the domain
    """
    _default_domain.change_domain_variable(variable,values)

class SubDomain(Domain):
    """A subdomain is a domain together with a specified subset of the domain variables.

    A subdomain implicitly represents all functions from joint
    instantiations of the domain variables whose values depend only on
    the specified subset of the domain variables. As such they can represent data-less
    L{Parameters.Factor} object.

    @ivar _variables: The specified subset of domain variables.
    @type _variables: frozenset
    """

    def __init__(self,variables=(),domain=None,new_domain_variables=None,
                 must_be_new=False,check=False):
        """Construct a L{SubDomain} object

        @param variables: The subset of domain variables for the object.
        @type variables: Iterable
        @param domain: A domain for the model.
        If None the internal default domain is used.
        If the string 'new', a new empty domain is used.
        @type domain: L{Domain} or None
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
        if domain is None:
            domain = _default_domain
        elif domain == 'new':
            domain = Domain()
        Domain.__init__(self,domain,new_domain_variables,must_be_new)
        variables = frozenset(variables)
        if check and not variables.issubset(self._domain):
            raise VariableError("Variables %s not a subset of %s " %
                                (variables,frozenset(self._domain)))
        self._variables = variables

    def __add__(self, other):
        """Factor addition

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the addition
        @type other: Typically L{Parameters.Factor} or float object
        @return: Result of factor addition
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self.copy()._pointwise_op(other,operator.add)

    def __div__(self,other):
        """Division, typically of factors

        0/0 is defined to equal 1
        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the division
        @type other: Typically L{Parameters.Factor} object or float object
        @return: Result of factor division
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self.copy()._pointwise_op(other,extdiv)

    def __iadd__(self,other):
        """Does in-place addition

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the addition
        @type other: Typically L{Parameters.Factor} or float object
        @return: C{self} after being added
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self._pointwise_op(other, operator.add)

    def __idiv__(self,other):
        """Does in-place division

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the division
        @type other: Typically L{Parameters.Factor} or float object
        @return: C{self} after being divided
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self._pointwise_op(other,extdiv)

    def __imul__(self,other):
        """Does in-place multiplication

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the multiplication
        @type other: Typically L{Parameters.Factor} or float object
        @return: C{self} after being multiplied
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self._pointwise_op(other,operator.mul)

    def __isub__(self,other):
        """Does in-place subtraction

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the subtraction
        @type other: Typically L{Parameters.Factor} or float object
        @return: C{self} after being subtracted
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self._pointwise_op(other, operator.sub)

    def __mul__(self,other):
        """Factor multiplication

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the multiplication
        @type other: Typically L{Parameters.Factor} or float object
        @return: Result of factor multiplication
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self.copy()._pointwise_op(other,operator.mul)

    def __rdiv__(self,other):
        """Only called when evaluating other / self, where
        other is not a Factor, but self is

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Number
        @type other: Numeric
        @return: C{other} / C{self}
        @rtype: Same as C{self}
        @raise TypeError: If other is not a number
        """
        return self.copy()._pointwise_op(other,extdiv,swapped=True)

    def __sub__(self, other):
        """Factor subtraction

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Factor or scalar on the RHS of the subtraction
        @type other: Typically L{Parameters.Factor} or float object
        @return: Result of factor subtraction
        @rtype: Same as C{self}
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        return self.copy()._pointwise_op(other,operator.sub)

    def __repr__(self):
        return 'SubDomain(%s,%s)' % (self._variables,Domain.__repr__(self))

    def __str__(self):
        return str(dict((v,self._domain[v]) for v in self._variables))
    
    def __rmul__(self,other):
        """Only called when evaluating other*self, where
        other is not a Factor, but self is

        ('Factors' can be L{SubDomain} objects.) 
        @param other: Number
        @type other: Numeric
        @return: C{other} * C{self}
        @rtype: Same as C{self}
        @raise TypeError: If other is not a number

        """
        return self.copy()._pointwise_op(other,operator.mul,swapped=True)

    def copy(self,copy_domain=False):
        """Return a 'copy' of a subdomain

        @param copy_domain: If true C{self}'s domain is copied, otherwise the copy
        shares C{self}'s domain
        @type copy_domain: Boolean
        @return: The copy
        @rtype: L{SubDomain}
        """
        if copy_domain:
            domain = Domain.copy(self)
        else:
            domain = self
        return SubDomain(self._variables,domain)


    def insts(self,variables=None):
        """Return an iterator over joint instantiations of C{variables}

        Each instantiation is a tuple of values, the order of the values
        corresponding to the ordering of variables in C{variables}. The instantiations
        themselves follow the standard ordering.
        @param variables: Which variables to include. If None, all the table/factor's
        variables are included in order.
        @type variables: Sequence or None
        @return: An iterator over joint instantiations
        @rtype: Iterator
        @raise KeyError: If a variable in C{variables} is not defined.
        """
        if variables is None:
            variables = sorted(self._variables)
        return self._insts(variables)

    def insts_indices(self,variables=None):
        """Return an iterator over the indices of joint instantiations of C{variables}

        Each instantiation is a tuple of integers, the order of the values
        corresponding to the ordering of variables in C{variables}. The instantiations
        themselves follow the standard ordering.
        @param variables: Which variables to include. If None, all the table/factor's
        variables are included in order.
        @type variables: Sequence or None
        @return: An iterator over joint instantiations
        @rtype: Iterator
        @raise KeyError: If a variable in C{variables} is not defined.
        """
        if variables is None:
            variables = sorted(self._variables)
        return self._insts_indices(variables)

    def inst2index(self,inst):
        """Return the index associated with the instantiation C{inst}.

        The first time this is called a lookup table (dictionary) is created
        to enable subsequent calls to be returned by lookup.
        @param inst: A tuple of ordered values, one for each variable 
        @type inst: Tuple
        @return: The index associated with the instantiation C{inst}
        @rtype: Int
        @raise KeyError: If there is no joint instantiation C{inst}.
        """
        try:
            return self._inst2index[inst]
        except AttributeError:
            self._inst2index = {}
            for i, tmp_inst in enumerate(self.insts()):
                self._inst2index[tmp_inst] = i
            return self._inst2index[inst]

    def marginalise_away(self,variables):
        """Alter self by marginalising away  C{variables}

        C{variables} is not altered. Variables in C{variables}
        which are not in C{self}'s variables are ignored.
        @param variables: Variables to marginalise away
        @type variables: Sequence
        @return: The altered C{self}
        @rtype: Class of C{self}
        """
        self._variables = self._variables.difference(variables)
        return self
        

    def marginalise_onto(self,variables):
        """Alter self by marginalising onto the intersection of
        C{variables} and C{self}'s variables

        @param variables: Variables to keep
        @type variables: Sequence
        """
        return self.marginalise_away(self._variables.difference(variables))


    def table_size(self,variables=None):
        """Return the number of joint instantiations of C{variables} in C{self}

        @param variables: Variables for which we want number of joint instantiations.
        If None, all variables are considered.
        @type variables: Iterable or None
        @return: The number of joint instantiations of variables in C{self}
        @rtype: Integer
        """
        if variables is None:
            variables = self._variables
        return reduce(operator.mul,[self._numvals[v] for v in variables],1)

    def sumout(self,vars_togo):
        """Summing out (marginalising) the variables vars_togo from the factor

        Does not alter C{self}
        @param vars_togo: Variables to sum out
        @type vars_togo: Iterable, e.g. list, tuple, set
        @return: New factor with vars_togo summed out
        @rtype: L{Parameters.Factor} object
        """
        return self.copy().marginalise_away(vars_togo)

    def uses_default_domain(self):
        """Whether C{self} uses the internal default domain

        @return: Whether C{self} uses the internal default domain
        @rtype: Boolean
        """
        return self._domain is _default_domain._domain

    def variables(self):
        """Return the object's variables

        @return: The object's variables
        @rtype: Frozenset
        """
        return self._variables

    def varvalues(self):
        """Return the dictionary mapping the object's variables
        to their set of possible values

        @return: Map from variables to values
        @rtype: Dictionary
        """
        varvalues = {}
        for v in self._variables:
            varvalues[v] = self._domain[v]
        return varvalues

    def _decode_inst(self,inst):
        if isinstance(inst,(tuple,list)):
            return self.inst2index(tuple(inst))
        elif isinstance(inst,dict):
            vals = []
            for name in sorted(self._variables):
                val = inst[name]
                if val not in self._domain[name]:
                    raise KeyError("%s has no value called %s" % (name,val))
                else:
                    vals.append(val)
            return self.inst2index(tuple(vals))
        else:
            return inst

    
    def _insts(self,variables):
        if variables:
            for value in sorted(self._domain[variables[0]]):
                for inst in self._insts(variables[1:]):
                    yield ((value),) + inst
        else:
            yield ()

    def _insts_indices(self,variables):
        if variables:
            for i in range(self._numvals[variables[0]]):
                for inst in self._insts_indices(variables[1:]):
                    yield ((i),) + inst
        else:
            yield ()


    def _pointwise_op(self,other,op,swapped=False):
        """
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's values dictionary.
        """
        if not isinstance(other,(float,int)):
            self._variables = self._get_result_variables(other)
        return self

    def _get_result_variables(self,other):
        """
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's domain.
        """
        self.common_domain(other)
        return self._variables | other._variables

        
    
