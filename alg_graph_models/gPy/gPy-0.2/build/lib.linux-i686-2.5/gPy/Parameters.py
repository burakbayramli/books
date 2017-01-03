"""Parameters for hierarchical (including graphical) models

@var entry_width: Width in characters for windows allowing data entry.
@type entry_width: Int
@var epsilon: Tolerance when checking that probability distributions sum to 1.
@type epsilon: Float
@var precision: Precision for printing floating point numbers
@type precision: int
@var sep: String used to separate columns in textual representation of factors
@type sep: Int
@var _version: Version of this module
@type _version: String
"""

_version = '$Id: Parameters.py,v 1.19 2008/10/07 09:12:30 jc Exp $'

class ParameterError(Exception):
    """Base class for errors in the Parameters module"""
    pass

class DataError(ParameterError):
    pass

class CPTError(ParameterError):
    pass

import Tkinter
from Variables import SubDomain, extdiv, Domain
from copy import copy
import operator
try:
    import gPyC
except ImportError:
    print "No C functions"
import os

entry_width = 7
epsilon = 0.001
precision = 2
sep = ' | '

# class Sumlgammadiffs:
#     def __init__(self):
#         self.count = 0
        
#     def step(self, value, alpha):
#         self.count += gPyC.lgammadiff(value,alpha)

#     def finalize(self):
#         return self.count

class Factor(SubDomain):
    """Factors represent functions from a discrete product space to the reals

    Typically used to construct discrete probability distributions with
    certain conditional independence properties
    @ivar _data: A value for each joint instantiation of the factor's L{_variables}
    @type _data: List
    """

    def __init__(self,variables=(),data=None,domain=None,new_domain_variables=None,
                 must_be_new=False,check=False,convert=False):
        """Initialise a L{Factor} object

        @param variables: Variables in the factor 
        @type variables: Sequence
        @param data: The numbers associated with the instantiations of the
        variables. These are ordered to correspond with a depth-first
        ordering of the instantiations, so the final variable changes fastest. If None, then
        a list of 1.0s of the right size is created.
        @type data: List
        @param new_domain_variables: A dictionary containing a mapping from any new
        variables to their values.
        @type new_domain_variables: Dict or None
        @param domain: A domain for the model.
        If None the internal default domain is used.
        @type domain: L{Variables.Domain} or None
        @param must_be_new: Whether domain variables in C{new_domain_variables} have
        to be new
        @type must_be_new: Boolean
        @param check: Whether to check that
        (1) C{variables} is of the right form, and (2) that each variable
        has an associated set of values and (3) that C{data} is the right size and type.
        @type check: Boolean
        @param convert: If C{True}, C{data} is converted to a list. 
        @type convert: Boolean
        @raise TypeError:  If C{check} is set and C{convert} is not set and
        C{data} is of the wrong type.
        @raise VariableError: If C{check} is set and there is a variable in
        C{variables} which does not have
        associated values. Or If a variable in C{new_domain_variables}
        already exists with values different from
        its values in C{new_domain_variables};
        Or if C{must_be_new} is set and the variable already exists.
        @raise DataError: If C{check} is set and C{data} is the wrong size.
        """
        SubDomain.__init__(self,variables,domain,new_domain_variables,must_be_new,check)
        if data is None or check:
            size = 1
            # next line fixes a previous bug as suggested by Charles Blundell
            for variable in self._variables:
                size *= self._numvals[variable]
        if data is None:
            data = [1.0] * size
        if convert:
            data = list(data)
        if check:
            if size != len(data):
                raise DataError("Expected data of size %d, but got data of size %d" %
                                (size,len(data)))
            if not isinstance(data,list):
                raise TypeError("Data for a factor must be a list, try calling with convert = True")
        self._data = data
    

    def __getitem__(self,inst):
        """Return value associated with the instantiation C{inst}
        
        C{inst} is either a list/tuple of values, or a dictionary mapping
        variables to values, or a data index.
        Uses an internal dictionary which will be built if not already.
        @param inst: Instantiation
        @type inst: Tuple, List, Dictionary or Int
        @return: The value in C{self} associated with C{inst}
        @rtype: Float
        @raise KeyError: If there is no joint instantiation C{inst}.
        """
        return self._data[self._decode_inst(inst)]
    

    def __iter__(self):
        """Iterates over the factors in a factor

        @return: Iterator over factor
        @rtype: Iterator
        @raise ValueError: If C{self} has no variables.
        """
        top = min(self._variables)
        rest = self._variables - set([top])
        chunk_size = len(self._data) / self._numvals[top]
        for i in range(0,len(self._data),chunk_size):
            yield Factor(rest,self._data[i:i+chunk_size],self)

    def __len__(self):
        """Return the number of values in a factor

        @return: The number of values in a factor
        @rtype: Int
        """
        return len(self._data)

    def __repr__(self):
        return 'Factor(%s,%s,None,%s)' % (self._variables,
                                          self._data,
                                          self._domain)

    def __setitem__(self,inst,val):
        """Set value associated with the instantiation C{inst}
        
        C{inst} is either a list/tuple of values, or a dictionary mapping
        variables to values, or a data index.
        Uses an internal dictionary which will be built if not already.
        @param inst: Instantiation
        @type inst: Tuple, List, Dictionary or Int
        @return: The value in C{self} associated with C{inst}
        @rtype: Float
        """
        self._data[self._decode_inst(inst)] = val

    def __str__(self):
        """
        Pretty representation of a factor
        
        @return: Pretty representation of a factor
        @rtype: String
        """
        fmt, dashes = self._header()
        out = ('\n'
               + fmt % tuple(sorted(self._variables)) + '\n'
               + dashes + ('-' * (precision+2)) + '\n')
        if isinstance(self._data[0],int):
            rfmt = '%6d\n'
        elif isinstance(self._data[0],float):
            rfmt = '%%6.%df\n' % precision
        else:
            rfmt = '%s\n'
        for i, inst in enumerate(self.insts()):
            out += fmt % inst
            out += rfmt % self._data[i]
        return out

    def broadcast(self,variables):
        """Alter C{self} to be  a function of C{variables}

        Broadcasting data values as necessary
        @param variables: New variables for C{self}
        @type variables: Iterable
        @return: Altered self
        @rtype: L{Factor}
        @raise ValueError: If C{variables} is not a superset of C{self}'s variables
        """
        variables = frozenset(variables)
        if not (self._variables <= variables):
            raise ValueError("%s not a subset of  %s" % (self._variables,variables))
        self._data = self._data_broadcast(self._data,
                                          sorted(variables),
                                          len(variables) - len(self._variables))
        self._variables = variables
        return self

    def copy(self,copy_domain=False):
        """Return a 'copy' of a factor

        @param copy_domain: If true C{self}'s domain is copied, otherwise the copy
        shares C{self}'s domain
        @type copy_domain: Boolean
        @return: The copy
        @rtype: L{Factor}
        """
        cp = SubDomain.copy(self,copy_domain)
        cp.__class__ = Factor
        cp._data = self._data[:]
        return cp

    def copy_rename(self,oldnew,copy_domain=False):
        """Return a 'copy' of a factor, but using different variables
        
        For each variable pair old -> new, either:
        1) new is a known variable (see L{Variables.Domain.known_variable}) with
        the same values as old or
        2) new is a new variable; in which case it will be given the same values
        as old
        
        @param oldnew: A mapping from old to new variables
        @type oldnew: Dictionary
        @param copy_domain: If true C{self}'s domain is copied, otherwise the copy
        shares C{self}'s domain
        @type copy_domain: Boolean
        @return: The copy
        @rtype: L{Factor}
        @todo: Implement efficiently
        """
        cp = self.copy(copy_domain)
        
        for old, new in oldnew.items():
            if new in self._domain:
                if self._domain[old] != self._domain[new]:
                    raise ValueError(
                        "%s and %s have different values: %s and %s, resp."
                        % (new, old, self._domain[old], self._domain[new]))
            else:
                cp.add_domain_variable(new,self._domain[old])


        oldvars = sorted(self._variables)
        newvars = [oldnew[old] for old in oldvars]
        cp._variables = frozenset(newvars)

        # slow but simple
        for inst in self.insts():
            olddkt = dict(zip(oldvars,inst))
            newdkt = dict(zip(newvars,inst))
            cp[newdkt] = self[olddkt]
        return cp

    def data(self):
        """Return a copy of the data in a L{Factor}

        @return: A copy of the data in a L{Factor}
        @rtype: List
        """
        return self._data[:]

    def data_restrict(self,newvalues,keep_class=False):
        """Alter a factor's B{data} by effecting the restriction on
        variables given by C{newvalues}

        Note that this does not alter the factor's domain
        which has to therefore be done separately.
        @param newvalues: Dictionary of the form {var1:values1,var2:values2..}.
        Each value of this dictionary must be an iterable.
        Values in an iterable which were not previously values of the corresponding variable
        are ignored. Variables which are not used in the factor are also ignored. Upon return
        each value in C{newvalues.values()} will be a frozenset.
        @type newvalues: Dictionary
        @param keep_class: Object will become a L{Factor} if false, otherwise class is unaltered.
        @type keep_class: Boolean
        @return: C{self}
        @rtype: C{Factor}
        """
        togo = len(self._variables.intersection(newvalues))
        self._data = self._data_restrict(sorted(self._variables),togo,newvalues,self._data)
        if not keep_class:
            self.__class__ = Factor
        return self

    def data_extend(self,newvalues,keep_class=False):
        """Alter a factor's B{data} by introducting new values to
        a variable given by the dictionary C{newvalues}

        @param newvalues: Dictionary of the form {var1:values1,var2:values2..}.
        Each value of this dictionary must be an iterable.
        Values in an iterable which were not previously values of the corresponding variable
        are ignored. Variables which are not used in the factor are also ignored. Upon return
        each value in C{newvalues.values()} will be a frozenset.
        @type newvalues: Dictionary
        @param keep_class: Object will become a L{Factor} if false, otherwise class is unaltered.
        @type keep_class: Boolean
        @return: C{self}
        @rtype: C{Factor}
        @author: Charles Blundell
        """
        if set(newvalues.keys()) - self._variables:
            raise RuntimeError, 'Use .broadcast, not .data_extend'
        togo = len(self._variables.intersection(newvalues))
        before = len(self._data)
        self._data = self._data_extend(sorted(self._variables),togo,newvalues,self._data)
        after = len(self._data)
        assert before <= after
        if not keep_class:
            self.__class__ = Factor
        return self


    def demo_mult(self,other):
        """Explicit string representation of multiplying C{self} and C{other}

        Resulting factor has strings such as '0.2 * 0.5'
        instead of numbers as values, so cannot
        be used for much else apart from displaying.

        @param other: Factor on RHS of multiplication
        @type other: L{Factor}
        @return: Factor with strings as values
        @rtype: L{Factor}
        """
        def strmul(x,y):
            return '%s * %s' % (x,y)
        return self.copy()._pointwise_op(other,strmul)

    def differ(self,other,epsilon=0.0):
        """Whether there are corresponding values in C{self} and C{other} differing by
        more than C{epsilon}

        @param other: Factor
        @type other: L{Factor}
        @param epsilon: Permissible difference
        @type epsion: Float
        @return: Whether they differ
        @rtype: Boolean
        @raise ValueError: If the two factors do not have the same number of data values.
        """
        od = other._data
        sd = self._data
        if len(od) != len(sd):
            raise ValueError("Factors don't have the same number of data values")
        for i, x in enumerate(sd):
            if abs(x-od[i]) > epsilon:
                return True
        return False
    
    

    def gui_buttons(self,gui,data_indices,tkc_variables):
        """Return a frame of buttons to a GUI for manipulating a factor

        @param gui: The GUI into which the buttons will be packed
        @type gui: Frame
        @param data_indices: list of lists of indexes into a data list
        @type data_indices: List
        @param tkc_variables: List of lists of C{Tkinter.StringVar} objects corresponding
        to C{data_indices}
        @type tkc_variables: List

        """
        gui_buttons = Tkinter.Frame(gui)
        for (txt,cmd) in (('Clear',lambda vs=tkc_variables: self._gui_tkc_clear(vs)),
                          ('Reset',lambda ds=data_indices, vs=tkc_variables:
                           self._gui_tkc_set(ds,vs)),
                          ('Cancel',gui.destroy),
                          ('Done', lambda w=gui, ds=data_indices, vs=tkc_variables:
                           self._gui_tkc_done(w,ds,vs))):
            button = Tkinter.Button(gui_buttons,text=txt,command=cmd)
            button.bind('<Return>', lambda event: cmd())
            button.pack(side=Tkinter.LEFT)
        return gui_buttons

    def gui_display(self,parent,**config):
        """Display a GUI widget for displaying a factor

        @param parent: A widget into which the GUI is placed.
        @type parent: Some suitable Tk object.
        @param config: Optional extra configuration options to control how
        GUI widget is displayed in C{parent}.
        @type config: various
        """
        gui = Tkinter.Frame(parent)
        gui.pack(config)
        gui_main = self.gui_main(gui,edit=False)
        gui_main.pack()
        for (txt,cmd) in [('Done',gui.destroy)]:
            button = Tkinter.Button(gui,text=txt,command=cmd)
            button.bind('<Return>', lambda event: cmd())
            button.pack()
        

    def gui_edit(self,parent):
        """Display a GUI widget for editing a factor

        @param parent: A widget into which the GUI is placed.
        @type parent: Some suitable Tk object.
        """
        gui = Tkinter.Frame(parent)
        gui.pack()
        gui_main, data_indices, tkc_variables = self.gui_main(gui)
        gui_main.pack()
        gui_buttons = self.gui_buttons(gui,data_indices,tkc_variables)
        gui_buttons.pack()
        

    def gui_main(self,parent,edit=True,varselect=False,**config):
        """Return a C{Tkinter.Frame} (and possibly other information)
        for displaying and editing a factor

        If C{edit=True} then also return information needed for subsequent
        editing
        @param parent: The Frame's parent widget.
        @type parent: Some suitable Tk object.
        @param edit: If set, the Frame will allow editing of the factor's data
        @type edit: Boolean
        @param varselect: If set, the user can 'select' variables by clicking on their names
        @type varselect: Boolean
        @param config: Other parameters for the frame
        @type config: Dictionary
        @return: If C{edit=False} the frame widget. If C{edit=True} a tuple
        C{(frame,data_indices, tkc_variables)} where the latter two are both identically
        structured lists of lists; C{data_indices} indexes into the factor's data and
        C{tkc_variables} contains C{Tkinter.StringVar} objects one for each data point
        @rtype: Frame or Tuple
        """
        gui_main = Tkinter.Frame(parent,borderwidth=2,relief=Tkinter.GROOVE,**config)
        data_indices = [[i] for i in range(len(self._data))]
        if varselect:
            self._gui_header(gui_main,sorted(self._variables),Tkinter.Button)
        else:
            self._gui_header(gui_main,sorted(self._variables),Tkinter.Label)
        if edit:
            tkc_variables = self._gui_edit_rows(gui_main,self.insts(),data_indices,1)
            return gui_main, data_indices, tkc_variables
        else:
            self._gui_display_rows(gui_main,self.insts(),data_indices,1)
            return gui_main

    def h_score(self,ess=1.0):
        """Return h_score of factor with ESS = C{ess}

        This is the log of the function called H in UAI08 paper.

        Computes \sum_i log(\Gamma(n_i+\alpha)/\Gamma(\alpha))
        where alpha = ess/(no. of values in self)
        """
        from gPyC import lgh
        return lgh(self._data,ess/len(self._data))
        

    def inc_from_rawdata(self,rawdata):
        """
        Increment C{self} with counts directly from C{rawdata}

        OK for parameter fitting not for structure learning
        @param rawdata: A tuple like that returned by L{IO.read_csv}.
        @type rawdata: Tuple
        @raise IndexError: If C{self} has a variable missing from C{rawdata}
        """
        variables,records = rawdata[2:]
        variables_info = []
        size = len(self._data)
        for variable in sorted(self._variables):
            indx = variables.index(variable)
            step = size/self.numvals(variable)
            variables_info.append((indx,step))
            size = step
        for record in records:
            self.inc_from_record(variables_info,record)

    def inc_from_record(self,variables_info,record):
        """Increment a data value in a factor from a data record

        @param variables_info: A sequence of which each element
        corresponds to a variable in C{self} (variables in order).
        Each element is a pair C{(indx,step)} where

         0. C{indx} is the index of the field in C{record} corresponding
        to the variable

         1. C{step} is the number of data values corresponding to any given
        value of the variable
        @type variables_info: Sequence
        @param record: A data record (such as produced via L{IO.read_csv})
        @type record: Sequence
        """
        i = 0
        for variable_info in variables_info:
            i += record[variable_info[0]] * variable_info[1]
        self._data[i] += record[-1]

    def insts_data_tuple(self,keyfn=None):
        """Return instantiations with values

        Returns a tuple of pairs. Each pair is of the form (inst,val)
        each inst is a tuple giving a joint instantiation of the
        variables.

        If C{key} is not None, it should be a function which takes an
        (inst,val) object as input. The (inst,val) pairs are ordered according
        to the value of this function.
        For example key = f where def f(x): return x[1] orders by value

        If {key} is None, the order is the default one. Lexicographically on inst.

        @return: Instantiations with values ordered by value
        @rtype: Tuple
        """
        result = []
        data = self._data
        result = [(inst,data[i]) for i, inst in  enumerate(self.insts())]
        if keyfn is not None:
            result.sort(key=keyfn,reverse=True)
        return tuple(result)

    def map(self, fn, keep_class=False):
        """Transform by an arity 1 function.
        By default, C{self} is forced to a Factor, since rarely
        will C{fn} result in anything more specific. C{keep_class} controls
        this behaviour.

        @param fn: An arity 1 function
        @type fn: function between values of an element of the object (e.g., L{Parameters.Factor} values)
        @param keep_class: Do not force C{self} to be a L{Parameters.Factor}
        @type keep_class: Boolean (defaulting to False)
        @rtype: Same as C{self}
        @return: Result of applying the function
        """
        self._data = map(fn, self._data)
        if not keep_class:
            self.__class__ = Factor
        return self

    def marginalise_away(self,variables):
        variables = frozenset(variables)
        self._data = self._data_marginalise(
            self._data,
            sorted(self._variables),
            self._variables & variables)
        SubDomain.marginalise_away(self,variables)
        self.__class__ = Factor
        return self

    def makeCPT(self,child,cpt_check=True):
        """Construct an L{CPT} object from a factor

        by just naming C{child} as the child variable.
        Will raise an exception if the factor is not a CPT with C{child}
        as the child
        @param child: Variable of form (varname,(varvalue1,varvalue2,..))
        @type child: tuple
        @return: The CPT
        @rtype: L{CPT} object
        @raise CPTError: if the factor is not a CPT with C{child}
        """
        return CPT(self,child,cpt_check)

    def normalised(self):
        """Return a new factor with values proportional to those of self
        but which sum to one

        @return: Normalised version of C{self}
        @rtype: Factor
        """
        def fn(x): return extdiv(x,float(self.z()))
        return Factor(self._variables,map(fn,self._data),self)

    def repr_nodomain(self):
        """Return string representation but without domain

        @return: String representation but without domain given
        @rtype: String
        """
        return 'Factor(%s,%s)' % (self._variables,self._data)


    def z(self):
        """Return the sum of the factor's values

        @return: Sum of the factor's values
        @rtype: float
        """
        return reduce(operator.add,self._data)

    def zero(self):
        """Set all values for a factor to zero
        """
        self._data = [0] * len(self._data)


    def _data_broadcast(self,data,variables,n_extra):
        """Broadcast C{data} to make room for C{variables}

        @param data: A sublist of C{self}'s data corresponding to an
        instantiation of a prefix of C{self}'s ordered variables.
        @type data: List
        @param variables: An ordered sequence of variables which is a superset of
        the suffix of C{self}'s ordered variables
        @type variables: Sequence
        @param n_extra: How many variables in C{variables} are not in the remaining
        suffix of C{self}'s variables
        @type n_extra: Int
        @return: Broadcast data
        @rtype: List
        @raise KeyError: If C{variables} contains a variable which is not in C{self}'s
        values dictionary
        """
        if n_extra == 0:
            return data
        variable, rest = variables[0], variables[1:]
        if variable in self._variables:
            chunk_size = len(data)/self._numvals[variable]
            newdata = []
            for i in range(0,len(data),chunk_size):
                newdata.extend(self._data_broadcast(data[i:i+chunk_size],rest,n_extra))
            return newdata
        else:
            return self._data_broadcast(data,rest,n_extra-1) * self._numvals[variable]

    def _data_marginalise(self,data,variables,to_sumout):
        """Sum out variables C{to_sumout} from C{data}

        Variables in C{to_sumout} not in C{variables} have no
        effect
        @param data: Data
        @type data: List
        @param variables: The (ordered) variables for C{data}
        @type variables: Sequence
        @param to_sumout: Variables to sum out
        @type to_sumout: Set/frozenset
        @return: Data with C{to_sumout} variables summed out
        @rtype: List
        """
        if not to_sumout:
            return data
        if to_sumout.issuperset(variables):
            return [reduce(operator.add,data)]
        variable, rest = variables[0], variables[1:]
        chunk_size = len(data)/self._numvals[variable]
        if variable in to_sumout:
            new_to_sumout = to_sumout - set([variable])
            newdata = self._data_marginalise(data[:chunk_size],
                                                   rest,
                                                   new_to_sumout)
            for i in range(chunk_size,len(data),chunk_size):
                newdata = map(operator.add,
                              newdata,
                              self._data_marginalise(data[i:i+chunk_size],
                                                     rest,
                                                     new_to_sumout))
        else:
            newdata = []
            for i in range(0,len(data),chunk_size):
                newdata.extend(
                    self._data_marginalise(data[i:i+chunk_size],rest,to_sumout))
        return newdata


    def _data_restrict(self,variables,togo,newvalues,data):
        """Alter C{data} to effect the restriction given by C{newvalues}

        @param variables: The (sorted) variables for C{data}
        @type variables: Sequence
        @param togo: How many restrictions remain to be done
        @type togo: Int
        @param newvalues: Mapping from some variables to a subset of their original values
        @type newvalues: Dictionary
        @param data: The data to be restricted
        @type data: List
        @return: The data with the restriction given by C{newvalues} effected
        @rtype: List
        """
        if togo == 0:
            return data
        variable, rest = variables[0], variables[1:]
        chunk_size = len(data)/self._numvals[variable]
        newdata = []
        if variable in newvalues:
            test = False
            togo -= 1
            these_newvalues = frozenset(newvalues[variable])
        else:
            test = True
        for i, value in enumerate(sorted(self._domain[variable])):
            if test or value in these_newvalues:
                newdata.extend(
                    self._data_restrict(rest,togo,
                                        newvalues,data[i*chunk_size:(i+1)*chunk_size]))
        return newdata

    def _data_extend(self,variables,togo,newvalues,data):
        """Alter C{data} to effect the extension given by C{newvalues}

        @param variables: The (sorted) variables for C{data}
        @type variables: Sequence
        @param to_add: How many extensions remain to be done
        @type to_add: Int
        @param newvalues: Mapping from some variables to a subset of their original values
        @type newvalues: Dictionary
        @param data: The data to be restricted
        @type data: List
        @return: The data with the restriction given by C{newvalues} effected
        @rtype: List
        @author: Charles Blundell
        """
        if togo == 0:
            return data
        variable, rest = variables[0], variables[1:]
        chunk_size = len(data)/self._numvals[variable]
        newdata = []
        old_values = sorted(self._domain[variable])
        if variable not in newvalues:
            for i, value in enumerate(old_values):
                chunk = data[i*chunk_size:(i+1)*chunk_size]
                newdata.extend(self._data_extend(rest,togo,newvalues,chunk))
            return newdata

        togo -= 1
        values = sorted(newvalues[variable])
        for value in values:
            if value in self._domain[variable]:
                i = old_values.index(value)
                chunk = data[i*chunk_size:(i+1)*chunk_size]
            else:
                chunk = [0]*chunk_size

            newdata.extend(self._data_extend(rest,togo,newvalues,chunk))

        return newdata

        


    def _factor_pointwise_op(self,other,op):
        """Apply binary operation C{op} to the factors C{self} and C{other}

        Copies of C{self} and C{other} are 'broadcast' as necessary
        C{self} is altered to contain the result.
        C{self} and C{other} will have identical values dictionaries after
        this method has been executed (Normally they already are identical).
        @param other: Factor on the RHS of the operation
        @type other: L{Factor}
        @param op: A binary operation which takes numeric arguments
        @type op: Function object
        @raise VariableError: If C{self} and C{other} use a variable with different values
        in each one's values dictionary.
        """
        result_variables = self._get_result_variables(other)
        n_self_extra = len(result_variables - self._variables)
        n_other_extra = len(result_variables - other._variables)
        ordered_result_variables = sorted(result_variables)
        self._data = map(
            op,
            self._data_broadcast(self._data,ordered_result_variables,n_self_extra),
            other._data_broadcast(other._data,ordered_result_variables,n_other_extra)
            )
        self._variables = result_variables
    

    def _gui_display_rows(self,gui_main,insts,data_indices,hrows):
        """Add rows of joint instantiations and corresponding data
        values for each to a factor's main GUI

        @param gui_main: The GUI into which the rows will be added
        @type gui_main: C{Tkinter.Frame}
        @param insts: The joint instantiations (one for each row)
        @type insts: Sequence (of sequences of strings)
        @param data_indices: list of lists of indexes into a data list
        @type data_indices: List
        @param hrows: Number of header rows, and thus the
        vertical offset for these rows
        @type hrows: Int
        """
        for r, inst in enumerate(insts):
            r2 = r + hrows
            if inst:
                for c, value in enumerate(inst):
                    Tkinter.Label(gui_main,text=value,relief=Tkinter.GROOVE,
                          justify=Tkinter.LEFT).grid(row=r2,column=c,sticky=Tkinter.NSEW)
                c += 1
            else:
                Tkinter.Label(gui_main,text='',relief=Tkinter.GROOVE,
                          justify=Tkinter.LEFT).grid(row=r2,column=0,sticky=Tkinter.NSEW)
                c = 1
            for i, data_index in enumerate(data_indices[r]):
                Tkinter.Label(gui_main,text=str(self._data[data_index]),
                      relief=Tkinter.RAISED).grid(row=r2,column=c+i,sticky=Tkinter.NSEW)

    def _gui_edit_rows(self,gui_main,insts,data_indices,hrows):
        """Add rows of joint instantiations and data entry widgets

        Initialises widgets to show current data values
        @param gui_main: The GUI into which the rows will be added
        @type gui_main: C{Tkinter.Frame}
        @param insts: The joint instantiations (one for each row)
        @type insts: Sequence (of sequences of strings)
        @param data_indices: list of lists of indexes into a data list
        @type data_indices: List
        @param hrows: Number of header rows in C{gui_main}, and thus the
        vertical offset for these rows
        @type hrows: Int
        @return: List of lists of C{Tkinter.StringVar} objects corresponding
        to C{data_indices}
        @rtype: List
        """
        tkc_variables = []
        for r, inst in enumerate(insts):
            r2 = r + hrows
            if inst:
                for c, value in enumerate(inst):
                    Tkinter.Label(gui_main,text=value,relief=Tkinter.GROOVE,
                          justify=Tkinter.LEFT).grid(row=r2,column=c,sticky=Tkinter.NSEW)
                c += 1
            else:
                Tkinter.Label(gui_main,text='',relief=Tkinter.GROOVE,
                      justify=Tkinter.LEFT).grid(row=r2,column=0,sticky=Tkinter.NSEW)
                c = 1
            tkc_variables_row = []
            for i, data_index in enumerate(data_indices[r]):
                var = Tkinter.StringVar()
                Tkinter.Entry(gui_main,textvariable=var,
                      width=entry_width).grid(row=r2,column=c+i,sticky=Tkinter.NSEW)
                var.set(str(self._data[data_index]))
                tkc_variables_row.append(var)
            tkc_variables.append(tkc_variables_row)
        return tkc_variables

    def _gui_header(self,gui_main,variables,widget_type,firstcol=0,r=0,span=1):
        """Add button or label headers to a factor's main GUI window

        Buttons are bound to the L{_gui_select} method.
        @param gui_main: The main GUI window to which buttons/labels are to be added
        @type gui_main: C{Tkinter.Frame}
        @param variables: Sequence of strings providing text for the labels/buttons
        @type variables: Sequence
        @param widget_type: Either Tkinter.Button or Tkinter.Label
        @type widget_type: Tkinter.Button or Tkinter.Label Tkinter class object
        @param firstcol: The column into which the first label/button goes
        @type firstcol: Int
        @param r: The row for the buttons/labels
        @type r: Int
        @param span: The column span of each button/label
        @type span: Int
        """
        widgets = []
        for c, variable in enumerate(variables):
            widget = widget_type(gui_main,text=variable,
                                 relief=Tkinter.RAISED,justify=Tkinter.LEFT)
            widget.grid(row=r,column=c+firstcol,columnspan=span,sticky=Tkinter.NSEW)
            widgets.append(widget)
        if widget_type == Tkinter.Button:
            for button in widgets:
                button.config(command=lambda b=button: self._gui_select(b))

    @staticmethod

    def _gui_select(button):
        """Change the appearance of a button

        @param button: The button
        @type button: C{Tkinter.Button}
        """
        if button['relief'] == Tkinter.RAISED:
            button.config(relief=Tkinter.SUNKEN,fg='red',activeforeground='red')
        else:
            button.config(relief=Tkinter.RAISED,fg='black',activeforeground='black')

    @staticmethod

    def _gui_tkc_clear(tkc_variables):
        """Clear the a list of lists of Entry widgets

        @param tkc_variables: List of lists of C{Tkinter.StringVar} objects,
        typically corresponding to factor data indices
        @type tkc_variables: List

        """
        for tkc_variables_row in tkc_variables:
            for var in tkc_variables_row:
                var.set('')

    def _gui_tkc_done(self,win,data_indices,tkc_variables):
        """Change a factor's data to be that contained in a list of lists of
        C{Tkinter.StringVar} objects and destroy the window C{win}

        @param win: Window to destroy
        @type win: Suitable Tkinter object
        @param data_indices: list of lists of indexes into a data list
        @type data_indices: List
        @param tkc_variables: List of lists of C{Tkinter.StringVar} objects corresponding
        to C{data_indices}
        @type tkc_variables: List

        """
        self._gui_tkc_get(data_indices,tkc_variables)
        win.destroy()

    def _gui_tkc_get(self,data_indices,tkc_variables):
        """Change a factor's data to be that contained in a list of lists of
        C{Tkinter.StringVar} objects

        @param data_indices: list of lists of indexes into a data list
        @type data_indices: List
        @param tkc_variables: List of lists of C{Tkinter.StringVar} objects corresponding
        to C{data_indices}
        @type tkc_variables: List

        """
        for r, tkc_variables_row in enumerate(tkc_variables):
            data_indices2 = data_indices[r]
            for i, var in enumerate(tkc_variables_row):
                self._data[data_indices2[i]] = float(var.get())

    def _gui_tkc_set(self,data_indices,tkc_variables):
        """Set the data displayed by a list of lists of
        C{Tkinter.StringVar} objects to be the factor's
        data

        @param data_indices: list of lists of indexes into a data list
        @type data_indices: List
        @param tkc_variables: List of lists of C{Tkinter.StringVar} objects corresponding
        to C{data_indices}
        @type tkc_variables: List

        """
        for r, tkc_variables_row in enumerate(tkc_variables):
            data_indices = data_indices[r]
            for i, var in enumerate(tkc_variables_row):
                var.set(str(self._data[data_indices[i]]))

    def _header(self,variables=None):
        """Return a format string and string of dashes suitable for creating a
        header for rows of instantiations of C{variables}

        @param variables: Variables for which a header is sought. If C{None}
        then C{self}'s variables are used.
        @type variables: (Ordered) sequence
        @return: The format string and string of dashes
        @rtype: Tuple
        """
        if variables is None:
            variables = sorted(self._variables)
        fmt = dashes = ''
        def lenstr(x): return len(str(x))
        for varname in variables:
            width = max(len(str(varname)),max(map(lenstr,self.values(varname))))
            fmt += '%%-%ss%s' % (width,sep)
            dashes += '-' * width + sep
        return fmt, dashes
            

    def _pointwise_op(self,other,op,swapped=False):
        """Apply binary operation C{op} to the factor C{self} and C{other}.
        C{self} is altered to contain the result

        After this operation has been applied, C{self} is always set to have
        class L{Factor}, since if it were originally a L{CPT} there is no guarantee
        that it still is one.

        C{other} may be a factor or a scalar
        @param other: Factor on the RHS of the operation
        @type other: L{Factor}, Float or Int
        @param op: A binary operation which takes numeric arguments
        @type op: Function object
        @param swapped: If set and C{other} is a number return
        C{other op self}
        @type swapped: Boolean
        @return: C{self}
        @rtype: L{Factor}
        """
        if isinstance(other,(float,int)):
            if swapped:
                def fn(x): return op(other,x)
            else:
                def fn(x): return op(x,other)
            self._data = map(fn,self._data)
        else:
            self._factor_pointwise_op(other,op)
        self.__class__ = Factor
        return self


        

class CPT(Factor):
    """Conditional probability tables"""

    def __init__(self,factor,child,
                 cpt_check=False,cpt_force=False,allow_dummies=False):
        """Initialise an L{CPT} object

        @param factor: The CPT as a L{Factor}
        @type factor: L{Factor} object 
        @param child: The child of the CPT
        @param cpt_check: Whether to check that this factor is indeed
        a CPT with C{child} as child
        @type cpt_check: Boolean
        @param cpt_force: Whether to convert the factor into a CPT
        by normalisation
        @type cpt_force: Boolean
        @param allow_dummies: If C{cpt_force=True}, whether to put a dummy row
        when when normalisation produces a division by zero.
        @type allow_dummies: Boolean
        @raise ZeroDivisionError: If C{cpt_force=True, allow_dummies=False}
        and normalisation produces a division by zero
        """
        self.__dict__ = factor.__dict__
        self._child = child
        if cpt_check and child not in self._variables:
            raise CPTError(
                "Child %s missing from %s" % (child,self._variables))
        if cpt_check or cpt_force:
            for data_indices in self.parent_insts_indices():
                prob_sum = 0.0
                for indx in data_indices:
                    prob_sum += self._data[indx]
                if abs(1-prob_sum) > epsilon:
                    if cpt_force:
                        if allow_dummies and not prob_sum > 0:
                            for indx in data_indices:
                                self._data[indx] = 0.0
                        else:
                            for indx in data_indices:
                                self._data[indx] /= prob_sum
                    else:
                        errmsg = '\nFor child:\t%s\n' % child
                        #def fn((var,val)): return '='.join((var,str(val)))
                        errmsg += 'For row:\t%s\n' % data_indices
                        errmsg += 'Sum was:\t%4.2f (should be 1.0)\n' % prob_sum
                        #raise CPTError(errmsg)
                        raise CPTError(errmsg)

    def __getitem__(self,inst):
        if (isinstance(inst,(tuple,list,dict)) and
            len(inst) == len(self._variables) - 1):
            if isinstance(inst,dict):
                inst = [inst[name] for name in sorted(self.parents())]
            inst = tuple(inst)
            it = self.parent_insts_data()
            for pi in self.parent_insts():
                data = it.next()
                if pi == inst:
                    return CPT(Factor([self._child],data,self),self._child)
        else:
            return Factor.__getitem__(self,inst)

    def __repr__(self):
        return 'CPT(%s,%s)' % (Factor.__repr__(self),
                               repr(self._child))

    def __str__(self):
        """ 
        Pretty representation of a CPT
        
        @return: Pretty representation of a CPT
        @rtype: String
        """
        parents = sorted(self._variables - set([self._child]))
        fmt, dashes = self._header(parents)
        rwidth = 0
        rfmt = childvalheader = ''
        for childvalue in sorted(self._domain[self._child]):
            width = max(6,len(str(childvalue))+2)
            childvalheader += '%*s' % (width,childvalue)
            dashes += '-' * width
            rfmt += '%%%d.%df' % (width,precision)
            rwidth += width
        rfmt += '\n'
        out = ('\n'
               + (fmt % tuple(parents)) + str(self._child).center(rwidth) + '\n'
               + (fmt % tuple([''] * len(parents))) + childvalheader + '\n'
               + dashes + '\n')
        itr = self.parent_insts_indices()
        for parent_inst in self.parent_insts():
            out += fmt % parent_inst
            out += rfmt % tuple([self._data[i] for i in itr.next()])
        return out

    def bdeu_score(self,precision=1.0):
        """Return the BDeu component score corresponding to C{self}

        This is the score for the family represented by the parents and child
        of C{self} and where the values of C{self} are interpreted as data counts for the
        relevant instantiations.

        The BDeu score for a Bayesian network is the sum of component scores for each of its CPTs.
        
        @param precision: The prior precision used to set the Dirichlet parameters
        @type precision: Float
        @return: BDeu score for family
        @rtype: Float
        @raise NameError: If L{gPyC} was not successfully imported
        """
        return gPyC.family_bdeu(self._getinterval(),self._numvals[self._child],
                           self._data,precision)
 

    def child(self):
        """Return the child in a CPT

        @return: The child in a CPT
        @rtype: Immutable (usually String)
        """
        return self._child

    def copy(self,copy_domain=False):
        return CPT(Factor.copy(self,copy_domain),self._child)

    def copy_rename(self,oldnew,copy_domain=False):
        return CPT(Factor.copy_rename(self,oldnew,copy_domain),
                   oldnew[self._child])
        

    def get_counts(self,data):
        """Return a 'CPT' with same structure as C{self} but with appropriate
        counts from C{data} as its values

        @param data: The data where the counts are
        @type data: L{CompactFactor}
        @return: A 'CPT' with counts from C{data} as values
        @rtype: L{CPT}
        """
        return data.makeFactor(self._variables).makeCPT(self._child,cpt_check=False)

    def get_counts_sql(self,data):
        """Return a 'CPT' with same structure as C{self} but with appropriate
        counts from C{data} as its values

        @param data: The data where the counts are
        @type data: L{SqliteFactor}
        @return: A 'CPT' with counts from C{data} as values
        @rtype: L{CPT}
        """
        return data.makeFactor(self._variables).makeCPT(self._child,cpt_check=False)


    def gui_buttons(self,gui,data_indices,tkc_variables):
        gui_buttons = Factor.gui_buttons(self,gui,data_indices,tkc_variables)
        for (txt,cmd) in [('Normalise',lambda vs=tkc_variables: self._gui_tkc_normalise(vs))]:
            button = Tkinter.Button(gui_buttons,text=txt,command=cmd)
            button.bind('<Return>', lambda event: cmd())
            button.pack(side=Tkinter.LEFT)
        return gui_buttons

    def gui_main(self,parent,edit=True,varselect=False,**config):
        gui_main = Tkinter.Frame(parent,borderwidth=2,relief=Tkinter.GROOVE,**config)
        data_indices = [data_row for data_row in self.parent_insts_indices()]
        if varselect:
            widget_type = Tkinter.Button
        else:
            widget_type = Tkinter.Label
        parents = sorted(self.parents())
        if not parents:
            parents = ['']
        self._gui_header(gui_main,parents,widget_type)
        self._gui_header(gui_main,[self._child],widget_type,firstcol=len(parents),
                         r=0,span=self._numvals[self._child])
        self._gui_header(gui_main,sorted(self.values(self._child)),
                         Tkinter.Label,firstcol=len(parents),
                         r=1)
        if edit:
            tkc_variables = self._gui_edit_rows(gui_main,self.parent_insts(),data_indices,2)
            return gui_main, data_indices, tkc_variables
        else:
            self._gui_display_rows(gui_main,self.parent_insts(),data_indices,2)
            return gui_main

    def k2(self):
        """Return the K2 component score corresponding to C{self}

        The score used for Cooper and Herskovits's K2 algorithm.

        The K2 score for a Bayesian network is the sum of component scores for each of its CPTs.
        
        @return: K2 component score
        @rtype: Float
        @raise NameError: If L{gPyC} was not successfully imported
        """
        # K2 score is just marginal log-likelihood with all Dirichlet parameters = 1
        return gPyC.family_llh(self._getinterval(),self._numvals[self._child],
                               self._data,[1.0]*len(self._data))

    def llh(self,priors):
        """Return the marginal log-likelihood component score corresponding to C{self}

        This is the marginal log-likelihood component score for the
        family represented by the parents and child of C{self} and
        where the values of C{self} are interpreted as data counts for
        the relevant instantiations.

        Since there are no constraints on C{priors} likelihood equivalence may not obtain and so
        this score need not be a BDe score.

        The marginal log-likelihood for a Bayesian network is the sum of component scores for each of its CPTs.
        
        @param priors: A L{CPT} object with the same structure as C{self}
        where each value is the Dirichlet parameter for its corresponding instantiation.
        @type priors: L{CPT} object
        @return: Marginal log-likelihood component score
        @rtype: Float
        @raise NameError: If L{gPyC} was not successfully imported
        """
        return gPyC.family_llh(self._getinterval(),self._numvals[self._child],
                              self._data,priors._data)

    def parents(self):
        """Return the parents in a CPT

        @return: The parents in a CPT
        @rtype: Frozenset
        """
        return self._variables - set([self._child])

    def parent_insts(self):
        """Return an iterator over the joint instantiations of the parents
        in a CPT

        @return: Iterator over parent instantiations
        @rtype: Iterator
        """
        return self.insts(sorted(self._variables - set([self._child])))

    def parent_insts_data(self):
        """Return an iterator where each iteration is the 
        data corresponding to the corresponding parent instantiation

        @return: Iterator over rows of the data 
        @rtype: Iterator
        """
        for indices in self.parent_insts_indices():
            yield [self._data[i] for i in indices]

    def parent_insts_indices(self):
        """Return an iterator where each iteration is a list of
        data indices corresponding to the corresponding parent instantiation

        @return: Iterator over data indices
        @rtype: Iterator
        """
        interval = self._getinterval()
        step = interval * self._numvals[self._child]
        for start in range(0,len(self._data),step):
            for j in range(start,start+interval,1):
                yield range(j,j+step,interval)

    def marginalise_away(self,variables):
        if list(variables) == [self._child]:
            self._data = [1.0] * (len(self._data)/self._numvals[self._child])
            self._variables = self._variables - set([self._child])
            del self._child
            self.__class__ = Factor
        else:
            Factor.marginalise_away(self,variables)
        return self

    def repr_nodomain(self):
        """Return string representation but without domain

        @return: String representation but without domain given
        @rtype: String
        """
        return 'CPT(%s,%s)' % (Factor.repr_nodomain(self),repr(self._child))

    def _getinterval(self):
        """Return how far apart child values (for a given
        parent configuration) are in C{self}'s data
        """
        interval = len(self._data)
        for variable in sorted(self._variables):
            interval /= self._numvals[variable]
            if variable == self._child:
                return interval

    @staticmethod

    def _gui_tkc_normalise(tkc_variables):
        """Normalise rows for a CPT GUI

        @param tkc_variables: List of lists of C{Tkinter.StringVar} objects,
        typically corresponding to factor data indices
        @type tkc_variables: List
        """
        for tkc_variables_row in tkc_variables:
            total = 0.0
            nums = []
            for var in tkc_variables_row:
                num = float(var.get())
                total += num
                nums.append(num)
            for i, var in enumerate(tkc_variables_row):
                var.set(str(nums[i]/total))

