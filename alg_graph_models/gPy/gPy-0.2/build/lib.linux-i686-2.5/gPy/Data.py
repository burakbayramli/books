"""For storing and retrieving data (contingency tables)

@var _version: Version of this module
@type _version: String
"""

_version = '$Id: Data.py,v 1.2 2008/10/07 08:57:06 jc Exp jc $'


from Variables import SubDomain, extdiv, Domain
from math import log
from Parameters import Factor, CPT
import operator

class Data2(SubDomain):
    """Attempt to implement ADTrees as a single flat dictionary"""
    
    def __init__(self,rawdata,domain=None,rmin=200):
        """Initialise a from C{rawdata}
        """
        new_domain_variables, variables, records = rawdata[1:]
        SubDomain.__init__(self,variables,domain,new_domain_variables,check=True)
        self._data = {}
        self._make_dict(0,variables,records,rmin,self._data)


    def _make_dict(self,i,variables,records,rmin,data):
        """
        Each key is of form eg (None,1,None,2) indicates a
        branch corresponding to variable1=1 and variable3=2
        and all others summed out

        Each value is a tuple of tuples. Each individual tuple is
        the instantiation of the remaining variables and finally
        the count

        Note that each split has a None branch, corresponding
        to all values for the variable given by that depth
        """
        #print 'Working on', records[0][:i], records

        # if not too many records, then don't sum any counts
        # just store each count with appropriate key
        if i >= len(variables) or len(records) < rmin:
            tmp = []
            for record in records:
                tmp.append(record[i:])
            data[record[:i]] = tuple(tmp)
                #data[record[:-1]] = record[-1]
            return

        # split records according to ith value
        # and sum out ith value on the way
        split = []
        for val in self._domain[variables[i]]:
            split.append([])
        i_summed_out = {}
        for record in records:
            split[record[i]].append(record)
            # record[:i] constant for all records
            try:
                i_summed_out[record[i+1:-1]] += record[-1]
            except KeyError:
                i_summed_out[record[i+1:-1]] = record[-1]

        marginal_records = []
        common_prefix = records[0][:i]+(None,)
        for key, value in i_summed_out.items():
            marginal_records.append(common_prefix+key+(value,))

        # find most common value
        best_so_far = 0
        for j, list_of_records in enumerate(split):
            #print j, len(list_of_records)
            if len(list_of_records) > best_so_far:
                best_so_far = len(list_of_records)
                mcv = j

        # for all values apart from most common
        # send associated records to recursive call for processing
        for j, list_of_records in enumerate(split):
            if j != mcv and len(list_of_records) > 0:
                self._make_dict(i+1,variables,list_of_records,rmin,data)
        
        self._make_dict(i+1,variables,marginal_records,rmin,data)

    def _test(self,variables):
        variables = frozenset(variables)
        lv = len(variables)
        indx = []
        for i, v in enumerate(sorted(self._variables)):
            if v in variables:
                indx.append((i,self._numvals[v]))
                if len(indx) == lv:
                    break
        # find all matching keys:
        template = [None] * len(self._variables)
        for inst in self.insts_indices(sorted(variables)):
            # make query
            for j in range(len(inst)):
                temp = template[:]
                for i, ins in enumerate(inst):
                    if i != j:
                        temp[indx[i][0]] = ins
                    query = tuple(temp)
                    print query, 
                    for l in range(len(template)+1):
                        if query[:l] in self._data:
                            print query[:l], 'OK'
                            break


    def marginal(self,variables):
        """Return dict mapping non-zero insts (as indices) to values
        """
        variables = frozenset(variables)
        lv = len(variables)
        indx = []
        for i, v in enumerate(sorted(self._variables)):
            if v in variables:
                indx.append((i,self._numvals[v]))
                if len(indx) == lv:
                    break
        dkt = {}
        self._marginal((),indx,dkt)
        return dkt

    def _marginal(self,branch,vindices,dkt):
        """Return a dictionary mapping insts of C{variables} to
        their values (where non-zero)

        counts are conditional on the inst C{branch}
        """

        
        # first check to see if this branch has been pruned
        # (or exists exactly)
        branch_length = len(branch)
        nvars = len(self._variables)
        if not vindices:
            branch += tuple([None]*(nvars-branch_length))
            branch_length = len(branch)
            assert branch_length == nvars
        for i in range(branch_length+1):
            if branch[:i] in self._data:
                if not vindices:
                    # previously over-extended
                    branch = branch[:i]

                # make marginal dictionary
                vis = [v[0]-branch_length for v in vindices]
                tmp = {}
                for row in self._data[branch[:i]]:
                    key = tuple(row[k] for k in vis)
                    try:
                        tmp[key] += row[-1]
                    except KeyError:
                        tmp[key] = row[-1]
                template = [None]*(len(row)-1)
                for key, val in tmp.items():
                    for p, k in enumerate(vis):
                        template[k] = key[p]
                    assert len(branch+tuple(template)) == nvars
                    dkt[branch+tuple(template)] = val
                    print dkt
                return


        vi, numvals = vindices[0]
        # summing out all variables up to vi
        branch += tuple([None]*(vi-len(branch)))


        # for each i have to artificially extend
        # new_branch to check it's there, due
        # to artificial nature of flattened tree
        dkts = []
        for i in range(numvals):
            tmp = {}
            new_branch = branch+(i,)
            check_branch = new_branch
            while len(check_branch) < nvars:
                if check_branch in self._data:
                    # so not the mcv, so do normal recursive call
                    self._marginal(new_branch,vindices[1:],tmp)
                    break
                else:
                    check_branch += (None,)
            else:
                # missing branch for i
                mcv = i
                self._marginal(branch+(None,),vindices[1:],tmp)
            dkts.append(tmp)

        #correct dkts[mcv] by subtraction
        tmp_mcv = dkts[mcv]
        endbit = vi+1
        for i, tmp in enumerate(dkts):
            if i != mcv:
                for key, val in tmp.items():
                    tmp_mcv[branch+(None,)+key[endbit:]] -= val
        # fix keys in dkts[mcv]
        for key, val in tmp_mcv.items():
            tmp_mcv[branch+(mcv,)+key[endbit:]] = val
            del tmp_mcv[key]
        # put stuff in main dictionary
        for tmp in dkts:
            dkt.update(tmp)

        
class Data(SubDomain):
    """Factors whose data is stored in a table in an sqlite database

    There is one row in the table for each non-zero value. These are meant to
    be used for factors with many variables, but not too many non-zero values:
    contingency tables for example. This object is a sensible choice when most of the
    information required from the data can be gleaned in a few passes over it.

    At present all database are stored in RAM.

    @cvar db: The common database for objects of this class
    @type db: sqlite3.Connection object
    @ivar table: The name of table containing the object's data. This is read-only
    (it is defined by a 'property') and is always equal to: 'table%d' % id(self)
    @type table: String
    @ivar n: Number of datapoints in the data
    @type n: Integer
    
    """
    try:
        from pysqlite2 import dbapi2 as sqlite
        db = sqlite.connect(':memory:')
        cursor = db.cursor()
    except ImportError:
        print "Can't use Data class"
    # missing file at present in slackware!
    # import sqlite3



    def __init__(self,data=None,variables=(),
                 domain=None,new_domain_variables=None,
                 must_be_new=False,check=False,convert=False):
        """Initialise a L{Data} object

        @param data: If C{None}, then an empty L{Data} object is created.
        If a file object, then assumed to be connected to a CSV file in the format
        that L{IO.read_csv} can read.
        If a string, assumed to be the name of a CSV file in the format
        that L{IO.read_csv} can read.
        If a tuple, assumed to be like one returned by L{IO.read_csv}.

        Unless C{data} is None any values for C{variables} and
        C{new_domain_variables} are ignored.
        @type data: Tuple
        @param variables: Variables in the data
        @type variables: Sequence
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
        """
        from gPy.IO import read_csv
        if data is not None:
            data_type = type(data)
            if data_type == tuple:
                rawdata = data
            elif data_type == str:
                if data.endswith('.gz'):
                    import gzip
                    rawdata = read_csv(gzip.open(data))
                else:
                    rawdata = read_csv(open(data))
            elif data_type == file:
                rawdata = read_csv(data)
            else:
                raise TypeError('data should be file, string, tuple or None')
            new_domain_variables, variables, records = rawdata[1:]
        SubDomain.__init__(self,variables,domain,new_domain_variables,must_be_new,check)
        cols = ','.join([v + ' INT' for v in sorted(self._variables)]+['value INT'])
        self.cursor.execute('CREATE TABLE %s (%s)' % (self.table,cols))
        if data is not None:
            self.populate(records)
        self._n = self.total_count()
        self._cached_tables = []

    def __del__(self):
        self.cursor.execute('DROP TABLE IF EXISTS %s' % self.table)

    def __getstate__(self):
        dkt = self.__dict__.copy()
        self.cursor.execute('SELECT * FROM %s' % self.table)
        # add extra attribute for the data
        dkt['_data'] = self.cursor.fetchall()
        return dkt

    def __iter__(self):
        """Iterates over those joint instantiations of the data which have
        non-zero counts associated with them

        On each iteration a tuple of non-negative integers is returned. The final
        number is the count, the preceding numbers encode the joint instantiation.
        For example, (1,0,2,23) states that the instantiation (1,0,2) has occurred 23 times.
        (1,0,2) is the first variable with its instantiation 1 (its 2nd), the second with
        instantiation 0 (its 1st) and the third with instantiation 2 (its 3rd). Variables and
        values are ordered lexicographically.
        
        @return: Iterator over non-zero count instantiations
        @rtype: Iterator
        """
        self.cursor.execute('SELECT * FROM %s' % self.table) 
        for row in self.cursor:
            yield row

    def __setstate__(self,state):
        self.__dict__ = state
        cols = ','.join(sorted(self._variables)+['value'])
        self.cursor.execute('CREATE TABLE %s (%s)' % (self.table, cols))
        sql = 'INSERT INTO %s ( %s ) VALUES ( %s )' % (
            self.table,cols,','.join(['?']*(len(self._variables)+1)))
        self.cursor.executemany(sql, self._data)
        del self._data

    def h_score(self,precision=1.0):
        """Return the L{gPyC.lgh} score for entire data set

        @param precision: BDe-like precision
        @type precision: Float
        @return: L{gPyC.lgh} score
        @rtype: Float
        """
        from gPyC import lgh
        self.cursor.execute('SELECT value FROM %s' % self.table)
        data = [val[0] for val in self.cursor]
        return lgh(data,precision/self.table_size())

    def marginal(self,variables):
        """Return the marginal dataset containing only C{variables}

        Returned Data object will have same domain as C{self}

        Does not alter C{self}
        @param variables: Variables in returned marginal table
        @type variables: Iterable, e.g. list, tuple, set
        @return: New marginal dataset
        @rtype: L{Data} object
        """
        marginal = Data(None,variables,domain=self)
        if variables:
            cols = ','.join(sorted(marginal._variables))
            sql = ('INSERT INTO %s SELECT %s, sum(value) FROM %s GROUP BY %s' %
                   (marginal.table,cols,self.table,cols))
        else:
            sql = ('INSERT INTO %s SELECT sum(value) FROM %s' %
                   (marginal.table,self.table))
        marginal.cursor.execute(sql)
        return marginal


    def conditional_entropy(self,x,y):
        """
        Return the conditional entropy H(x|y) for variable sets C{x} and C{y}
        using the empirical distribution given by the data
        """
        return self.entropy(frozenset(x)|frozenset(y)) - self.entropy(y)


##    def get_rows(self,x):
##        """Equivalent to:

##        sql = 'SELECT sum(value) FROM %s GROUP BY %s' % (self.table,','.join(sorted(x)))
##        self.cursor.execute(sql)
##        return self.cursor.fetchall()
##        """
##        xset = frozenset(x)
##        # smaller tables always before bigger ones
##        for cached_table in self._cached_tables:
##            if cached_table == xset:
##                self.cursor.execute('SELECT value FROM table%s' % '_'.join(cached_table))
##                return self.cursor.fetchall()
##            if cached_table > xset:
##                cols = ','.join(sorted(xset))
##                table_name = 'table%s' % '_'.join(xset)
##                sql.cursor.execute('CREATE TABLE %s (%s,value)' % (table_name cols))
##                sql.cursor.execute('INSERT INTO %s SELECT %s, sum(value) FROM table%s GROUP BY %s' %
##                                   (table_name, cols, '_'.join(cached_table), cols))
                
                                
##                sql = 'SELECT sum(value) FROM table%s GROUP BY %s' % ('_'.join(cached_table),','.join(sorted(x)))
##                break
##        else:
##            sql = 'SELECT sum(value) FROM %s GROUP BY %s' % (self.table,','.join(sorted(x)))
##        l = len(xset)
##        for i, cached_table in enumerate(self._cached_tables):
##            if len(cached_table) >= l:
##                self._cached_tables.insert(i,xset)
##                break
##        else:
##            self._cached_tables.append(xset)
##        print 'CREATE TABLE table%s AS %s' % ('_'.join(xset),sql)
##        self.cursor.execute('CREATE TABLE table%s AS %s' % ('_'.join(xset),sql))
##        self.cursor.execute('SELECT value FROM table%s' % '_'.join(xset))
##        return self.cursor.fetchall()

    def entropy(self,x):
        """Return the entropy of the marginal empirical distribution given by C{x}
        and the data
        """
        #print frozenset(x),
        try:
            return self.entropy_cache[frozenset(x)]
        except AttributeError:
            pass
        except KeyError:
            pass
            #print 'UNcached'

        # empty set of variables has zero entropy
        if not x:
            return 0.0
        sql = 'SELECT sum(value) FROM %s GROUP BY %s' % (self.table,','.join(sorted(x)))
        self.cursor.execute(sql)
        rows = self.cursor.fetchall()
        #rows = self.get_rows(x)
        # if only one instantiation then entropy is zero
        # this shortcut avoids numerical problems
        if len(rows) == 1:
            try:
                self.entropy_cache[frozenset(x)] = 0.0
            except AttributeError:
                pass
            return 0.0
        h = sum(count * log(count) for count in (val[0] for val in rows))
        n = self._n
        entropy  = log(n) - h/n
        try:
            self.entropy_cache[frozenset(x)] = entropy
        except AttributeError:
            pass
        return entropy

    def _bic_search2(self,child,n,child_df,old_parents,further_parents,store,pa_lim,highest_llh):
        if len(old_parents) > pa_lim:
            return
        from heapq import heappush, heappop
        if further_parents:
            new_parent = further_parents[0]
            further_parents = further_parents[1:]

            for new_parents in old_parents, old_parents|frozenset([new_parent]):

                # for small parent sets don't bother trying to prune
                if len(new_parents) < 2:
                    self._bic_search2(child,n,child_df,new_parents,further_parents,store,pa_lim,highest_llh)
                else:
                    # is it worth continuing the branch without/with the new parent?
                    # note that a new pruning opportunity may arise even if new_parents == old_parents
                    # since the reduction in further_parents may decrease highest_llh enough
                    #highest_llh = -n * self.conditional_entropy([child],new_parents.union(further_parents))
                    lowest_penalty = log(n) * (child_df * self.table_size(new_parents)) / 2
                    best_possible = highest_llh - lowest_penalty
                    tmp = store[:]
                    # negation of the score is stored in the heap
                    score, scored_parents = heappop(tmp)
                    while -score > best_possible:
                        if scored_parents < new_parents:
                            break
                        score, scored_parents = heappop(tmp)
                    else:
                        self._bic_search2(child,n,child_df,new_parents,further_parents,store,pa_lim,highest_llh)
        else:
            dim = child_df * self.table_size(old_parents)
            complexity_penalty = (log(n) * dim) / 2
            bic_score = (-n * self.conditional_entropy([child],old_parents)
                         - complexity_penalty)
            if not store:
                store.append((-bic_score,old_parents))
                return
            tmp = store[:]
            # negation of the score is stored in the heap
            score, scored_parents = heappop(tmp)
            while -score > bic_score:
                if scored_parents < old_parents:
                    break
                score, scored_parents = heappop(tmp)
            else:
                heappush(store,(-bic_score,old_parents))

    def _bic_search(self,child,n,child_df,lower,bic_lower,upper,upper_bound,store):
        """
        Compute the BIC score for every parent set for C{child} which is a proper
        superset of C{lower} and a subset of the union of C{lower} and C{upper} and add it to the
        dictionary C{store}. C{bound} is a bound on the
        log-likelihood (modulo an additive constant) on any possible parentset
        C{bic_lower} is the BIC score of C{lower}.
        """
        # compute open list
        open_list = []
        for v in upper:
            parentset = lower | frozenset([v])
            dim = child_df * self.table_size(parentset)
            complexity_penalty = (log(n) * dim) / 2
            best_possible = upper_bound - complexity_penalty
            if best_possible > bic_lower:
                bic_score = (-n * self.conditional_entropy([child],parentset)
                             - complexity_penalty)
                if bic_score > bic_lower:
                    store[parentset] = bic_score
                    print 'Stored', parentset, bic_score
                    open_list.append((bic_score,parentset,v))
                else:
                    open_list.append((bic_lower,parentset,v))
            else:
                print 'Pruned', parentset
        open_list.sort()
        for bic_score, parentset, v in open_list:
            upper = upper - frozenset([v])
            self._bic_search(child,n,child_df,parentset,bic_score,upper,upper_bound,store)

            

    def bic_search(self,child,pa_lim):
        """Branch and bound search for all parent sets for C{child}
        which do not have a higher scoring subset

        TODO: use a random graph as an input
        """
        n = self._n
        child_df = self._numvals[child] - 1
        old_parents = frozenset()
        further_parents = tuple(self._variables - frozenset([child]))
        store = []
        highest_llh = -n * self.conditional_entropy([child],further_parents)
        self._bic_search2(child,n,child_df,old_parents,further_parents,store,pa_lim,highest_llh)
        return dict((s[1],-s[0]) for s in store)
        
##        lower = frozenset()
##        child_singleton = frozenset([child])
##        n = self._n
##        child_df = self._numvals[child] - 1
##        bic_lower = -n * self.entropy(child_singleton)-(0.5 * log(n) * child_df)
##        upper =  frozenset(self._variables - child_singleton)
##        upper_bound = -n * self.conditional_entropy(child_singleton,upper)
##        print 'Upper bound', upper_bound
##        store = {lower:bic_lower}
##        self._bic_search(child,n,child_df,lower,bic_lower,upper,upper_bound,store)
##        return store


##        from gPy.Utils import subsets_ascending
##        potential_parents = self._variables - frozenset([child])
##        n = self._n
##        upper_bound = -n * self.conditional_entropy([child],potential_parents)
##        #print 'Upper bound on log-likelihood component is', upper_bound
##        child_df = self.numvals(child) - 1
##        pruned = set()
##        bic_scores = {frozenset():(-n * self.entropy([child]) - (0.5 * log(n) * child_df))}

##        for parentset in subsets_ascending(potential_parents):
##            print 'Considering', parentset

##            # already done empty set
##            if not parentset:
##                continue

##            # if a superset of an already pruned parentset
##            # then don't consider
##            ok = True
##            for pruned_parentset in pruned:
##                if parentset > pruned_parentset:
##                    ok = False
##                    break
##            if not ok:
##                print 'Pruned'
##                continue
            
##            # 'size' method returns number of joint instantiations
##            # perhaps it needs a better name
##            dim = child_df * self.size(parentset)
##            best_possible = upper_bound - (0.5 * log(n) * dim)
##            best_subset_score_parentset = None
##            for done_parentset, score in bic_scores.items():
##                if done_parentset < parentset:
##                    if best_possible < score:
##                        pruned.add(parentset)
##                        break
##                    elif best_subset_score_parentset is None or best_subset_score_parentset < score:
##                        best_subset_score_parentset = score
##            else:
##                bic_score = (-n * self.conditional_entropy([child],parentset)
##                             - (0.5 * log(n) * dim))
##                if bic_score > best_subset_score_parentset:
##                    bic_scores[parentset] = bic_score
##                    print parentset, bic_score
##        return bic_scores

    def loglikelihood(self,adg):
        """The log-likelihood of C{adg} with MLE parameters (up to an
        additive constant)

        The missing constant is the log of the multinomial
        coefficient which is the  same for all adgs.
        
        """
        dkt = {}
        for child in adg.vertices():
            parents = frozenset(adg.parents(child))
            family = parents | frozenset([child])

            try:
                dkt[parents] += 1
            except KeyError:
                dkt[parents] = 1

            try:
                dkt[family] -= 1
            except KeyError:
                dkt[family] = -1

        for variableset, mutliplier in dkt.items():
            if multiplier != 0:
                score += multiplier * self.entropy(variableset)
        return self.n * score

    def bic_complexity_penalty(self,adg):
        return (log(self.n/2.0) *
                sum((self.numvals(child) - 1) * self.table_size(adg.parents(child)) for child in adg.vertices()))
        
    def mutual_information(self,x,y):
        """Return the mutual information between the variable sets
        C{x} and C{y} in the empirical distribution
        determined by the data

        @param x: Variable set
        @type x: Iterable, frozenset most efficient
        @param y: Variable set
        @type y: Iterable, frozenset most efficient
        @return: The mutual information
        @rtype: Float
        @raise ValueError: If C{x} and C{y} are not disjoint
        @raise TypeError: If either C{x} or C{y} are not iterables
        """
        if not x or not y:
            return 0.0
        return self.entropy(x) + self.entropy(y) - self.entropy(frozenset(x)|frozenset(y))
        

    def total_count(self):
        """Return the number of datapoints in the data

        @rtype: The number of datapoints in the data
        @return: Integer
        """
        self.cursor.execute('SELECT sum(value) FROM %s' % self.table)
        return self.cursor.fetchone()[0]
        

    def populate(self,records,variables=None):
        """Simply inserts the records into the database

        Each record is a tuple of integers. Each integer, except the last, corresponds to a value.
        The last value is the count. If a joint instantiation occurs more than once, only the
        last count is used.
        
        Assumes lexicographic order of variables if C{variables} is None, otherwise the
        order given by C{variables}.

        """
        if variables is None:
            variables = sorted(self._variables)
        elif frozenset(variables) != self._variables:
            raise ValueError('Got sent these variables: %s, expected these: %s'
                             % (variables, self._variables))
        cols = ','.join(list(variables)+['value'])
        sql = 'INSERT INTO %s ( %s ) VALUES ( %s )' % (
            self.table,cols,','.join(['?']*(len(self._variables)+1)))
        self.cursor.executemany(sql, records)
        self._n = self.total_count()

    def qhs(self):
        self.cursor.execute('SELECT value FROM %s' % self.table)
        data = sorted([val[0] for val in self.cursor.fetchall()])
        hs = [0] * data[-1]
        m = len(data)
        j = 0
        for k, count in enumerate(data):
            for i in range(j,count):
                hs[i] = m - k
            j = count
        return hs

    def ub(self,qpa,alpha,ri):
        """The upper bound self provides on a the score of a smaller
        parent set where

        @param qpa: Size of contingency table for smaller parent set
        @alpha: Effective sample size
        @ri: Number of values of the child
        """
        hs = self.qhs()
        total = 0.0
        a = alpha/float(qpa)
        for h, qh in enumerate(hs):
            total += qh*log((h+a/ri)/(h+a))
            #print total
        return (float(qpa)/self.table_size())*total
        


    def qh(self,h=0):
        """Return the number of instantiations (ie cells) having a value
        greater than C{h}

        @param h: Threshold
        @type h: Integer
        """
        self.cursor.execute('SELECT value FROM %s WHERE value > %d' % (self.table,h))
        return len(self.cursor.fetchall())

    def make_family_scores_naively(self,pa_size_lim=4,precision=10.0,batch_size=65536):
        """
        Make scores for all parent sets for all variables where (1) the size
        of the parent set is at most C{pa_size_lim}. No pruning!
        """

        from gPy.Parameters import Factor
        from gPy.Utils import subsetn_batch
        from array import array
        from gPyC import lgh

        precision = float(precision)
        batch_size = min(batch_size,65536)
        
        marginal_size_lim = pa_size_lim+1
        cursor = self.db.cursor()

        h_score = {}
        pa_scores = {} # where it all ends up
        for v in self._variables:
            pa_scores[v] = {}

        if marginal_size_lim > len(self._variables)/2:
            raise ValueError('pa_size %d too big (see subseteqn_batch docs)' % pa_size)

        faset_batch_all = []
        def tmpfn(x): return len(x) == marginal_size_lim

        variable_indices = range(len(self._variables))
        last_variable_index = variable_indices[-1] # to cope with 'missing' subsets

        for faset_batch_all_tmp in subsetn_batch(variable_indices,marginal_size_lim,batch_size):

            # associate each variable set of size marginal_size_lim with 'its' subsets
            # subsets are attached to only one maximal sized set

            faset_batch_all.extend(faset_batch_all_tmp)

            #print len(faset_batch_all)

            faset_batch = filter(tmpfn,faset_batch_all)

            #print 'Done with subsets', len(faset_batch)
            # initialisation

            # with array('H') can store up to 65535

            fasets_i_including = [array('H') for var in self._variables]
            mults =   [{} for var in self._variables]
            current = ['0'] * len(self._variables)
            num_fasets = len(faset_batch)
            count = [None] * num_fasets

            indx = array('I',[0] * num_fasets)

            # compute stuff for each family set = marginal
                    
            for faset_i, faset in enumerate(faset_batch):
                size = 1
                for j in sorted(faset,reverse=True):    # the jth variable, j is an int
                    fasets_i_including[j].append(faset_i)
                    mults[j][faset_i] = size
                    size *= self._numvals[self._sortedvariables[j]]
                # dataset always smaller than  65535
                count[faset_i] = array('H',[0] * size)

            # processing data ...

            #print 'About to process data'
            
            cursor.execute('SELECT * FROM %s' % self.table)                            
            for val, valcount in cursor.fetchall():

                # compute indices (sparse matrix-vector computation)
                
                previous = current
                current = val.split(',')
                for j, inst_j in enumerate(current):
                    if inst_j == previous[j]:
                        continue
                    mult_j = mults[j]
                    diff_j = int(inst_j) - int(previous[j])
                    for faset_i in fasets_i_including[j]:
                        indx[faset_i] += (diff_j * mult_j[faset_i])

                # increment counts
                
                for faset_i, count_faset in enumerate(count):
                    count_faset[indx[faset_i]] += valcount

            # compute scores

            #print 'About to compute scores'


            # faset_batch contains subsets of marginal_size_lim
            # faset_batch_all contains all subsets *up to* marginal_size_lim

            k = 0
            for faset_i, faset in enumerate(faset_batch):
                subsets = []
                while True:
                    subset = faset_batch_all[k]
                    k += 1
                    subsets.append(subset)
                    if subset == faset:
                        break
                # to cope with some non-maximal size subsets coming just after
                # their containing maximal size subset
                if subsets[-1][-1] == last_variable_index:
                    biggest = frozenset(subsets[-1])
                    while k < len(faset_batch_all):
                        next_subset = faset_batch_all[k]
                        if not biggest.issuperset(next_subset):
                            break
                        subsets.append(next_subset)
                        k += 1
                #print subsets
                factor_data = list(count[faset_i])
                factor_variables = [self._sortedvariables[j] for j in faset]
                factor = Factor(factor_variables,factor_data,self)
                for marginal in subsets:  # marginal a set of variable indices
                    marginal_variables = frozenset([self._sortedvariables[j] for j in marginal]) 
                    marginal_data = (factor._data_marginalise(
                        factor_data,
                        factor_variables,
                        factor._variables - marginal_variables))
                    #print '\n^^^^^^^^^^^'
                    #print marginal_data
                    h_score[marginal_variables] = lgh(marginal_data,precision/len(marginal_data))
                    #print marginal_variables, h_score[marginal_variables]
            faset_batch_all = faset_batch_all[k:]

        for family, family_score in h_score.items():
            for child in family:
                parents = family-frozenset([child])
                pa_scores[child][parents] = family_score - h_score[parents]

        #  for testing
        #   for child, parentdict in pa_scores.items():
        #       for parent, score in parentdict.items():
        #           print child, parent, score,
        #           factor = self.makeFactor(parent | set([child]))
        #           print CPT(factor,child).bdeu_score(precision)

        return pa_scores

    def makeFactorsn(self,n,block=1000000):
        """Yield counts for all marginals with C{n} variables in blocks of C{block}

        Marginals are ordered according to how they are generated by the generator
        L{Utils.subseteqn}.

        @param n: The number of variables in the marginals
        @type n: Int
        @return: C{counts} where C{counts[i][idx]} is the count for the C{idx}th instantiation of
        the C{i}th marginal
        @rtype: List
        """
        
        from gPy.Utils import subseteqn
        
        marginals_including = [[] for var in self._variables]
        count = [None] * block
        mults = [{} for var in self._variables]
        for i, marginal in enumerate(subseteqn(range(len(self._variables)),n)):
            modi = i % block
            size = 1
            for j in sorted(marginal,reverse=True):    # the jth variable, j is an int
                marginals_including[j].append(modi)
                mults[j][modi] = size
                size *= self._numvals[self._sortedvariables[j]]
            count[modi] = [0] * size # so 'count' in synch with 'marginals'
            if block - modi == 1:
                print 'OK i is', i
                yield self._countsfromdata(count,mults,marginals_including)
                marginals_including = [[] for var in self._variables]
                count = [None] * block
                mults = [{} for var in self._variables]
        # for the tail end
        count = count[:modi+1]
        yield self._countsfromdata(count,mults,marginals_including)

    def _countsfromdata(self,count,mults,marginals_including):
        indx_template = [0] * len(count)
        cursor = self._data.cursor()
        cursor.execute('SELECT * FROM %s' % self.table)
        for val, valcount in cursor.fetchall():
            indx = indx_template[:]
            for j, inst_j in enumerate(eval(val)):
                mult_j = mults[j]
                for i in marginals_including[j]:
                     indx[i] += (inst_j * mult_j[i])
            for i, idx in enumerate(indx):
                count[i][idx] += valcount
        return count

    def makeFactorsn_old(self,n):
        """Return counts for all marginals with C{n} variables

        Marginals are ordered according to how they are generated by the generator
        L{Utils.subseteqn}.

        @param n: The number of variables in the marginals
        @type n: Int
        @return: C{counts} where C{counts[i][idx]} is the count for the C{idx}th instantiation of
        the C{i}th marginal
        @rtype: List
        """

        # each marginal represented by an ordered tuple of *indices*
        # 'id' of each marginal is just its index in the list 'marginals'
        # marginals = [x for x in subseteqn(range(len(self._variables)),n)]

        # marginals_including[j] contains (indexes of) all marginals containing jth variable
        # count[i] is a list big enough to contain marginal counts
        #   for ith marginal

        #for i, marginal in enumerate(marginals):

        # mults[j] contains multiplers for the jth variable for each
        # marginal containing it. Multipliers are ordered in synch
        # with marginals_including

        # count[i] will contain the counts for marginal i
        
        from gPy.Utils import subseteqn
        
        marginals_including = [[] for var in self._variables]
        mults = [{} for var in self._variables]

        count = []
        for i, marginal in enumerate(subseteqn(range(len(self._variables)),n)):
            size = 1
            for j in sorted(marginal,reverse=True):    # the jth variable, j is an int
                marginals_including[j].append(i)
                mults[j][i] = size
                size *= self._numvals[self._sortedvariables[j]]
            print i, size
            count.append([0] * size) # so 'count' in synch with 'marginals'

        print 'this long', len(count)
        indx_template = [0] * len(count)

        cursor = self._data.cursor()
        cursor.execute('SELECT * FROM %s' % self.table)
        for val, valcount in cursor.fetchall():
            indx = indx_template[:]
            for (inst_j,mult_j,interval_j,marginals_including_j) in zip(
                eval(val),mults,self._intervals,marginals_including):
                for i in marginals_including_j:
                    indx[i] += (inst_j * mult_j[i])
            for i, idx in enumerate(indx):
                count[i][idx] += valcount
        return count

    def family_score(self, child, parents, precision=1.0):
        return self.makeCPT(child, parents, force_cpt=False, check=False).bdeu_score(precision)


    def score_adg(self,adg,precision=1.0):
        """Get Bdeu score for an adg"""
        score = 0.0
        for child in adg.vertices():
            score += CPT(
                self.makeFactor(adg.parents(child) | set([child])),
                child).bdeu_score(precision)
        return score

    def h_scores(self,precision=1.0,textfun=str):
        from gPy.Utils import all_subsets
        from gPyC import lgh
        for varset in all_subsets(list(self._variables)):
            factor = self.makeFactor(varset)
            print len(varset), lgh(factor._data,precision/len(factor._data)), textfun(varset)

    def makeCPT(self, child, parents, force_cpt=False, check=False, prior=0):
        """
        @param prior: the Dirichlet prior parameter (the same parameter value
        is used for all instances!)  Note there may be some problems with
        this method: a B{different} prior is used by the BDeu score. However,
        in practice, for parameter estimation, this prior method seems to be ok.
        I was lazy and it was simple to implement (cb).  If prior is zero, then
        the parameters are the maximum likelihood estimation solutions.
        """
        family = set(parents) | set([child])
        f_child = self.makeFactor(family)
        return CPT(f_child+prior, child, cpt_check=check, cpt_force=force_cpt)

    
    def makeFactor(self,variables=None):
        """Simple way to get a factor

        @param variables: The variables in the required factor.
        If C{None}, then all of C{self}'s variables are used: which
        could produce a very large object!
        @type variables: Iterable
        @return: Marginal table
        @rtype: L{Factor} object
        """
        from gPy.Parameters import Factor
        
        if variables is None:
            marginal = self
        else:
            marginal = self.marginal(variables)
        marginal.cursor.execute('SELECT * FROM %s' % marginal.table)

        # compute step sizes for each variable
        st = 1
        step = []
        for v in sorted(variables,reverse=True):
            step.append(st)
            st *= self._numvals[v]
        step.reverse()

        data = [0] * marginal.table_size() 
        for row in self.cursor.fetchall():
            i = 0
            for j, val in enumerate(row[:-1]):
                i += val*step[j]
            data[i] = row[-1]
        return Factor(variables,data,domain=self)

    @property
    def n(self):
        """Return the current number of datapoints stored

        @return: The current number of datapoints stored
        @rtype: Integer
        """
        return self._n


    @property
    def table(self):
        """Return the name of the table storing C{self}'s data

        @return: The name of the table storing C{self}'s data
        @rtype: String
        """
        return 'table%d' % id(self)


    __getitem__ = makeFactor

#     def makeFactors(self,variablesets):
#         """Simple way to get factors"""
#         mults = []
#         indicess = []
#         datas = []
#         for variables in variablesets:
#             size = 1
#             mult = {}
#             mults.append(mult)
#             indices = []
#             indicess.append(indices)
#             for v in sorted(variables,reverse=True):    # the jth variable, j is an int
#                 mult[self._dkt[v]] = size
#                 indices.append(self._dkt[v])
#                 size *= self._numvals[v]
#             datas.append([0] * size)
         
#         cursor = self._data.cursor()
#         cursor.execute('SELECT * FROM %s' % self.table)
#         for val, valcount in cursor.fetchall():
#             inst = [int(x) for x in val.split(',')]
#             for i, variables in enumerate(variablesets):
#                 indx = 0
#                 for j in indicess[i]:
#                     indx += inst[j]*mults[i][j]
#                 datas[i][indx] += valcount

#         factors = []
#         for i, variables in enumerate(variablesets):
#             factors.append(Factor(variables,datas[i],domain=self))
#         return factors

        


#     def score_all(self,n,precision=1.0):
#         """Compute the 'lgamma' score associated with all marginal tables involving
#         C{n} variables.

#         Needs a better name!
#         """
#         from gPy.Utils import subseteqn
#         from array import array
#         indices_s = [x for x in subseteqn(range(len(self._variables)),n)]
#         tmp_numvals = []
#         for v in sorted(self._variables):
#             tmp_numvals.append(self._numvals[v])
#         dkts = []
#         intervals = [] 
#         for indices in indices_s:
#             dkts.append([0]*reduce(operator.mul,[tmp_numvals[j] for j in indices],1))
#             intervals.append([1]*10) ## WRONG!!
#         cur = self._con.cursor()
#         cur.execute('SELECT * FROM %s' % (self._table))
#         for row in cur.fetchall():
#             val = row[-1]
#             for i, indices in enumerate(indices_s):
#                 indx = 0
#                 for k, j in enumerate(indices):
#                     indx += row[j]*intervals[i][k]
#                 dkts[i][indx] += val
                        
#                 # perhaps make dkt a list and compute an integer index from [row[j] for j in indices]
#                 #for j in indices:
#                 #    dkt = dkt[row[j]]
#                 #dkt += val
#                 #key = tuple([row[j] for j in indices])
#                 #try:
#                 #    dkt[key] += val
#                 #except KeyError:
#                 #    dkt[key] = val

#         scores = [0.0] * len(dkts)
#         for i, dkt in enumerate(dkts):
#             scores[i] = gPyC.lgh(dkt,
#                                 reduce(operator.mul,[tmp_numvals[j] for j in indices_s[i]],1))
#         return scores, indices_s

#     def score(self,variables,precision=1.0):
#         """Compute the 'lgamma' score associated with the marginal table involving
#         variables C{variables}.

#         Needs a better name!
        
#         """
#         cur = self._con.cursor()
#         if variables:
#             cur.execute('SELECT sum(gpyval) FROM %s GROUP BY %s' % (
#                 self._table, ','.join(variables)))
#         else:
#             cur.execute('SELECT sum(gpyval) FROM %s' % self._table)
#         return gPyC.lgh(
#             [x[0] for x in cur.fetchall()],
#             precision/reduce(operator.mul,[self._numvals[v] for v in variables],1))

#    def bdeu_score(self,child,parents,precision=1.0):
#        """parents is a list"""
#        denom = self.score(parents,precision)
#        parents.append(child)
#        numer = self.score(parents,precision)
#        return numer - denom
        
    

class CompactFactor(SubDomain):
    """Factors whose data is not explicity represented (because there's
    too much of it). 

    Generally used to store datasets. Implemented using ADTrees
    @ivar _tree: ADTree holding the data
    @type _tree: L{_ADTree} object
    @ivar _records: Each record is a tuple of integers. For each tuple the M{j}th element
    is the index of the value of the M{j}th variable found in that record.
    The final integer is a count of how often the record appeared.
    @type _records: List
    @ivar _var_index: TODO
    """
    def __init__(self,data,domain=None,rmin=200):
        """Initialise a L{CompactFactor} from C{rawdata}

        @param data: See L{Data} documentation
        @type data: 
        @param domain: A domain for C{self}'s variables. If None
        the internal default domain is used.
        @type domain: Dictionary or None
        @param rmin: Controls the space/time tradeoff for using  L{CompactFactor} objects.
        Subsets of records smaller than C{rmin} are stored as a tuple of (pointers to) the records
        concerned.
        @type rmin: int
        @raise KeyError: if C{rawdata} has a variable that has not been previously declared
        @raise TypeError: if C{rawdata} is not a sequence or its elements are not of
        the right type.
        @raise ValueError: if C{rawdata} does not have exactly 3 elements
        """
        from gPy.IO import read_csv
        if data is not None:
            data_type = type(data)
            if data_type == tuple:
                rawdata = data
            elif data_type == str:
                if data.endswith('.gz'):
                    import gzip
                    rawdata = read_csv(gzip.open(data))
                else:
                    rawdata = read_csv(open(data))
            elif data_type == file:
                rawdata = read_csv(data)
            else:
                raise TypeError('data should be file, string, tuple or None')
        else:
            raise ValueError('Need some data actually!')
        new_domain_variables, variables, records = rawdata[1:]
        SubDomain.__init__(self,variables,domain,new_domain_variables,check=True)
        var_index = {}
        numvals = []
        for i, variable in enumerate(variables):
            var_index[variable] = i
            numvals.append(self._numvals[variable])
        n = 0
        for record in records:
            n += record[-1]
        self._tree = _ADTree(numvals,records,0,rmin)
        self._records = records
        self._var_index = var_index
        self._n = n

    def bic_search(self,child):
        """Branch and bound search for all parent sets for C{child}
        which do not have a higher scoring subset

        TODO: use a random graph as an input
        """
        n = self._n
        child_df = self._numvals[child] - 1
        old_parents = frozenset()
        further_parents = tuple(self._variables - frozenset([child]))
        store = []
        self._bic_search2(child,n,child_df,old_parents,further_parents,store)
        return dict((s[1],-s[0]) for s in store)

    def _bic_search2(self,child,n,child_df,old_parents,further_parents,store):
        from heapq import heappush, heappop
        if further_parents:
            new_parent = further_parents[0]
            further_parents = further_parents[1:]

            for new_parents in old_parents, old_parents|frozenset([new_parent]):

                # for small parent sets don't bother trying to prune
                if len(new_parents) < 2:
                    self._bic_search2(child,n,child_df,new_parents,further_parents,store)
                else:
                    # is it worth continuing the branch without/with the new parent?
                    # note that a new pruning opportunity may arise even if new_parents == old_parents
                    # since the reduction in further_parents may decrease highest_llh enough
                    highest_llh = -n * self.conditional_entropy([child],new_parents.union(further_parents))
                    lowest_penalty = log(n) * (child_df * self.table_size(new_parents)) / 2
                    best_possible = highest_llh - lowest_penalty
                    tmp = store[:]
                    # negation of the score is stored in the heap
                    score, scored_parents = heappop(tmp)
                    while -score > best_possible:
                        if scored_parents < new_parents:
                            break
                        score, scored_parents = heappop(tmp)
                    else:
                        self._bic_search2(child,n,child_df,new_parents,further_parents,store)
        else:
            dim = child_df * self.table_size(old_parents)
            complexity_penalty = (log(n) * dim) / 2
            bic_score = (-n * self.conditional_entropy([child],old_parents)
                         - complexity_penalty)
            if not store:
                store.append((-bic_score,old_parents))
                return
            tmp = store[:]
            # negation of the score is stored in the heap
            score, scored_parents = heappop(tmp)
            while -score > bic_score:
                if scored_parents < old_parents:
                    break
                score, scored_parents = heappop(tmp)
            else:
                heappush(store,(-bic_score,old_parents))

    def conditional_entropy(self,x,y):
        """
        Return the conditional entropy H(x|y) for variable sets C{x} and C{y}
        using the empirical distribution given by the data
        """
        return self.entropy(frozenset(x)|frozenset(y)) - self.entropy(y)

    def entropy(self,x):
        """Return the entropy of the marginal empirical distribution given by C{x}
        and the data
        """
        try:
            return self.entropy_cache[frozenset(x)]
        except AttributeError:
            pass
        except KeyError:
            pass
        
        # empty set of variables has zero entropy
        if not x:
            return 0.0
        #factor = self.makeFactor(x)
        h = sum(count * log(count) for count in self.get_nonzerocounts(x))
        n = self._n
        entropy  = log(n) - h/n
        try:
            self.entropy_cache[frozenset(x)] = entropy
        except AttributeError:
            pass
        return entropy



    def __str__(self):
        return "Variables: %s\nTree:\n %s\n" % (
            sorted(self._variables), self._tree)

    def makeCPT(self, child, parents, force_cpt=False, check=False, prior=0):
        """
        @param prior: the Dirichlet prior parameter (the same parameter value
        is used for all instances!)  Note there may be some problems with
        this method: a B{different} prior is used by the BDeu score. However,
        in practice, for parameter estimation, this prior method seems to be ok.
        I was lazy and it was simple to implement (cb).  If prior is zero, then
        the parameters are the maximum likelihood estimation solutions.
        """
        family = set(parents) | set([child])
        f_child = self.makeFactor(family)
        return CPT(f_child+prior, child, cpt_check=check, cpt_force=force_cpt)


    def get_nonzerocounts(self,variables):
        variables_info = [
            [self._var_index[var],self._numvals[var]]
            for var in variables]
        variables_info.sort()
        #size is the length of the list under each value of the corresponding variable
        size = 1
        for variable_info in reversed(variables_info):
            variable_info.append(size)
            size *= variable_info[1]
        return self._tree._flatten(variables_info,0,no_zeroes=True)
    
    def makeFactor(self,variables):
        """Return a marginal factor with C{variables}

        Unless C{variables} is empty in which case return
        the sum of C{self}'s data
        @param variables: Variables to project onto
        @type variables: Iterable
        @return: The marginal factor
        @rtype: L{Factor} object
        @raise KeyError: if C{variables} contains a variable not contained in the
        L{CompactFactor}.
        """
        variables_info = [
            [self._var_index[var],self._numvals[var]]
            for var in variables]
        variables_info.sort()
        #size is the length of the list under each value of the corresponding variable
        size = 1
        for variable_info in reversed(variables_info):
            variable_info.append(size)
            size *= variable_info[1]
        return Factor(variables,self._tree._flatten(variables_info,0),self)

    __getitem__ = makeFactor

    def tree_size(self):
        """Return the number of nodes in the underlying ADTree

        @return: The number of nodes in the tree
        @rtype: int
        """
        return self._tree.size()


class _ADTree(object):
    """ADTree implementation

    Ref::

     @Article{moore98:_cached_suffic_statis_effic_machin,
     author = 	 {Andrew Moore and Mary Soon Lee},
     title = 	 {Cached Sufficient Statistics for Efficient Machine Learning with Large Datasets},
     journal = 	 {Journal of Artificial Intelligence Research},
     year = 	 1998,
     volume =	 8,
     pages =	 {67--91},
     url = {http://www.jair.org/media/453/live-453-1678-jair.pdf}
     }

    @ivar _count: A count of the number of records 'in' the tree
    @type _count: int
    @ivar _data: may be
     .1 a tuple each element of which is either an _ADTree object or None,
     There is one element for each value of the variable corresponding to
     the top node of the tree. A None value indicates 0 records for the corresponding
     value
     .2 a tuple of records
     .3 a single _ADTree object. This is a space saving mechanism used when there
     is only one value of the variable with any records associated with it
    @type _data: Various
    @ivar _mcvindex: In cases 1) and 3) this is an integer stating which value has the most records.
    To this value *all* records are associated rather than just those with the appropriate
    value. In case 2) this is None
    @type _mcvindex: Various
    """

    __slots__ = ('_data','_count','_mcvindex')

    def __getstate__(self):
        return self._data, self._count, self._mcvindex

    def __setstate__(self,state):
        self._data, self._count, self._mcvindex = state


    def __init__(self,numvals,records,depth,rmin):
        """Initialise an L{_ADTree} object

        @param numvals: The ith element of this list is the number of values for the ith variable
        in the tree
        @type numvals: list
        @param records: The data as a list. Each element is a tuple of value indices, one for each variable,
        plus an extra count field as the final element.
        @type records: list
        @param depth: The depth of this tree within its containing CompactFactor. Also the index of the variable
        associated with the top node of this tree
        @type depth: int
        @param rmin: If the number of records is below C{rmin} then tree growing stops and C{records} is stored.
        Note that a single record may represent many datapoints since all records have an extra 'count' field.
        @type rmin: int
        """

        # all nodes have a count
        count = 0
        # each record has an extra 'count' field at the end
        for record in records:
            count += record[-1]
        self._count = count

        if depth == len(numvals):
            # no more variables to process
            self._data = ()
            self._mcvindex = None
            return

        if len(records) < rmin:
            # Case 2) above
            self._data = records
            self._mcvindex = None
            return

        #set up
        tmp = []
        counts = []
        for value in range(numvals[depth]):
            tmp.append([])
            counts.append(0)
        
        #distribute records
        for record in records:
            valindex = record[depth]
            tmp[valindex].append(record)
            counts[valindex] += record[-1]
                
        # find most common value
        mcvcount = 0
        for valindex, count in enumerate(counts):
            if count > mcvcount:
                self._mcvindex, mcvcount = valindex, count
        # mcv branch actually has all records
        # not just those for which first=mcv
        tmp[self._mcvindex] = records

        # make branches
        data = []
        found_not_empty_before = False

        for value_records in tmp:
            if value_records == []:
                data.append(None)
            else:
                data.append(_ADTree(numvals,value_records,depth+1,rmin))
                if found_not_empty_before:
                    one_not_empty = False
                else:
                    found_not_empty_before = True
                    one_not_empty = True

        if one_not_empty:
            # Case 3) above
            self._data = data[self._mcvindex]
        else:
            # Case 1) above
            self._data = tuple(data)

    def _flatten(self,variables_info,depth):
        """Return the data for a L{Factor}

        The L{Factor}'s variables will generally be a subset of those
        for which data is stored in the tree

        @param variables_info: Contains the necessary information on the variables
        sought without naming them. The ith element of C{variables_info} contains information on
        the ith variable sought. Each element of C{variables_info} is a 3 element list:
        variables_info[i][0] is the depth in the L{_ADTree} which deals with the ith variable sought.
        variables_info[i][1] is the number of values of the ith variable sought.
        variables_info[i][2] is the number of data values in the eventual factor which correspond
        to each value of the ith variable sought.
        (Clearly this depends on the number of values of 'later' variables.)
        @type variables_info: list
        @return: Values for the factor
        @rtype: list
        """
        if variables_info == []:
            # no need to descend tree further
            data = [self._count]
        elif self._mcvindex is None:
            # Case 2) above
            # self._data is a list of records
            # no need to descend tree further
            numvals = variables_info[0][1]
            size = variables_info[0][2]
            data = [0] * (numvals*size)
            for record in self._data:
                i = 0
                for variable_info in variables_info:
                    i += record[variable_info[0]] * variable_info[2]
                data[i] += record[-1]
        elif variables_info[0][0] == depth:
            # have reached the depth for the first variable
            numvals = variables_info[0][1]
            size = variables_info[0][2]
            mcvstart = self._mcvindex * size
            mcvend = mcvstart + size
            if isinstance(self._data,tuple):
                # Case 1) above
                data = []
                acc = [0] * size
                for i, branch in enumerate(self._data):
                    if branch is None:
                        data.extend([0] * size)
                    else:
                        this_data = branch._flatten(variables_info[1:],depth+1)
                        data.extend(this_data)
                        if i != self._mcvindex:
                            acc = map(operator.add,acc,this_data)
                # 'correct' data values for mcvindex
                data[mcvstart:mcvend] = map(operator.sub,data[mcvstart:mcvend],acc)
            else:
                # Case 3) above
                data = [0] * (size * numvals)
                data[mcvstart:mcvend] = self._data._flatten(variables_info[1:],depth+1)
        else:
            # Keep looking for the depth for first variable ..
            if isinstance(self._data,tuple):
                branch = self._data[self._mcvindex]
            else:
                branch = self._data
            data =  branch._flatten(variables_info,depth+1)
        return data


##    def _non_zerocounts(self,variables_info,depth):
##        """Return the data for a L{Factor}

##        The L{Factor}'s variables will generally be a subset of those
##        for which data is stored in the tree

##        @param variables_info: Contains the necessary information on the variables
##        sought without naming them. The ith element of C{variables_info} contains information on
##        the ith variable sought. Each element of C{variables_info} is a 3 element list:
##        variables_info[i][0] is the depth in the L{_ADTree} which deals with the ith variable sought.
##        variables_info[i][1] is the number of values of the ith variable sought.
##        variables_info[i][2] is the number of data values in the eventual factor which correspond
##        to each value of the ith variable sought.
##        (Clearly this depends on the number of values of 'later' variables.)
##        @type variables_info: list
##        @return: Values for the factor
##        @rtype: list
##        """
##        if variables_info == []:
##            # no need to descend tree further
##            if self._count > 0:
##                data = [self._count]
##            else:
##                data = []
##        elif self._mcvindex is None:
##            # Case 2) above
##            # self._data is a list of records
##            # no need to descend tree further
##            data = {}
##            for record in self._data:
##                key = tuple([record[variable_info[0]] for variable_info in variables_info])
##                try:
##                    data[key] += record[-1]
##                except KeyError:
##                    data[key] = record[-1]
##            data = data.values()
##        elif variables_info[0][0] == depth:
##            # have reached the depth for the first variable
##            numvals = variables_info[0][1]
##            size = variables_info[0][2]
##            mcvstart = self._mcvindex * size
##            mcvend = mcvstart + size
##            if isinstance(self._data,tuple):
##                # Case 1) above
##                data = []
##                acc = [0] * size
##                for i, branch in enumerate(self._data):
##                    if branch is None:
##                        data.extend([0] * size)
##                    else:
##                        this_data = branch._flatten(variables_info[1:],depth+1)
##                        data.extend(this_data)
##                        if i != self._mcvindex:
##                            acc = map(operator.add,acc,this_data)
##                # 'correct' data values for mcvindex
##                data[mcvstart:mcvend] = map(operator.sub,data[mcvstart:mcvend],acc)
##            else:
##                # Case 3) above
##                data = [0] * (size * numvals)
##                data[mcvstart:mcvend] = self._data._flatten(variables_info[1:],depth+1)
##        else:
##            # Keep looking for the depth for first variable ..
##            if isinstance(self._data,tuple):
##                branch = self._data[self._mcvindex]
##            else:
##                branch = self._data
##            data =  branch._flatten(variables_info,depth+1)
##        return data


    def size(self):
        """Return the number of nodes in the tree

        @return: The number of nodes in the tree
        @rtype: int
        """
        if self._mcvindex is None:
            return 1
        count = 0
        if isinstance(self._data,tuple):
            for branch in self._data:
                if branch is None:
                    count += 1
                else:
                    count += branch.size()
        else:
            count += self._data.size()
        return count

    def __str__(self):
        return "(%s,%s,[%s])\n" % (
            self._count,
            self._mcvindex,
            self._data)
        

def _merge_records(destination,source,how):
    """Given two sets of records, combing them according to the function C{how}
    @param destination: the target set of records where the result is stored
    and the left hand side of C{how}
    @type destination: list of tuples whose last element is a positive integer
    @param source: the right hand side of C{how}
    @type destination: list of tuples whose last element is a positive integer
    @param how: operator to combine C{destination} and C{source}
    @type how: binary function
    """
    # copy-on-write
    destination = list(destination)
    # merge together. (TODO: make faster by sorting and merging)
    for src_rec in source:
        src_prefix = src_rec[:-1]
        for i, dst_rec in enumerate(destination):
            if src_prefix == dst_rec[:-1]:
                destination[i] = destination[i][:-1] + (how(destination[i][-1], src_rec[-1]),)
                break
        else:
            destination.append(src_rec)
    return destination

def _distribute_records(numvals, depth, records):
    """For each child at depth C{depth} generate the list of
    records (C{children_records}) and the number of instances (C{counts}).
    @param numvals: The ith element of this list is the number of values for
    the ith variable in the tree
    @type numvals: list
    @param records: The data as a list. Each element is a tuple of value
    indices, one for each variable, plus an extra count field as the final
    element.
    @type records: iterable
    @param depth: The depth of this tree within its containing CompactFactor.
    Also the index of the variable associated with the top node of this tree
    @type depth: int
    @return: A 2-tuple: C{children_records} and C{counts}, as described above.
    @rtype: tuple
    """
    children_records = [[] for i in xrange(numvals)]
    counts = [0] * numvals
    for record in records:
        valindex = record[depth]
        children_records[valindex].append(record)
        counts[valindex] += record[-1]

    return children_records, counts

class _IncrementalADTree(_ADTree):

    __slots__ = ('_data','_count','_mcvindex')

    def __init__(self, numvals, records, rmin, depth=0):
        if numvals is None and records is None and rmin is None:
            # copy construct
            self._data = None
            self._count = None
            self._mcvindex = None
            return
        self._make_new_node(numvals, records, rmin, depth)

    def __str__(self, descend=True):
        s = '{%x:' % id(self)
        s += ' count: '+str(self._count)+', mcvindex: '+str(self._mcvindex)+', data: '
        if isinstance(self._data,tuple) or isinstance(self._data,list):
            s += '('+str(len(self._data))+')'
        if descend:
            if isinstance(self._data,tuple) or isinstance(self._data,list):
                s += '['
                for child in self._data:
                    if isinstance(child,_IncrementalADTree):
                        s += child.__str__(descend=False) + ' '
                    else:
                        s += str(child) + ' '
                s += ']'
            elif isinstance(self._data,_IncrementalADTree):
                s += self._data.__str__(descend=False)
            else:
                s += str(self._data)
        else:
            s += '...skipped...'
        s += '}'
        return s

    def str(self, numvals, rmin, n=0, mcv=True):
        prefix = str(n) + ' '*n
        s = prefix+'count: '+str(self._count)+', mcv:'+str(self._mcvindex)+'\n'
        if self._mcvindex is None:
            # record
            s += prefix+'records\n'
            s += prefix+str(self._data)+'\n'
        elif not isinstance(self._data,tuple):
            # singleton node
            s += prefix+'*'+str(self._mcvindex)+': singleton\n'
            if mcv:
                data = self._build_mcv(numvals, rmin, n+1)
            else:
                data = self._data
            if data is None:
                s += prefix+' '+'empty\n'
            else:
                s += data.str(numvals,rmin,n+1,mcv)
        else:
            # nodes
            for i, data in enumerate(self._data):
                if i == self._mcvindex:
                    s += prefix+'*'+str(i)+':\n'
                    if mcv:
                        data = self._build_mcv(numvals,rmin,n+1)
                else:
                    s += prefix+' '+str(i)+':\n'
                if data is None:
                    s += prefix+' '+'empty\n'
                else:
                    s += data.str(numvals,rmin,n+1,mcv)
        return s

    def copy(self):
        copy = _IncrementalADTree(None,None,None)
        copy._count = self._count
        copy._mcvindex = self._mcvindex
        if self._mcvindex is None:
            # records are copy-on-write (by _merge_records)
            copy._data = self._data
        else:
            if self._have_compact_children():
                copy._data = self._data.copy()
            else:
                copy._data = []
                for child in self._data:
                    if child is None:
                        copy._data.append(None)
                    else:
                        copy._data.append(child.copy())
                copy._data = tuple(copy._data)
        assert self._have_compact_children() == copy._have_compact_children()
        return copy

    def _compact_children(self, other_than_mcv=None):
        """Where possible convert self._data from a tuple (or list) into
        a singleton. other_than_mcv is a hint. If other than None, it indicates
        whether some child other than the MCV has a non-zero count."""
        if self._mcvindex is None:
            return

        if not isinstance(self._data,tuple) and not isinstance(self._data,list):
            return

        if other_than_mcv is None:
            for i, child in enumerate(self._data):
                if child is None or i == self._mcvindex:
                    continue
                other_than_mcv = True
                break
            else:
                other_than_mcv = False
        elif not other_than_mcv:
            for i, child in enumerate(self._data):
                if i == self._mcvindex:
                    continue
                assert child is None

        if not other_than_mcv:
            assert isinstance(self._data[self._mcvindex],_IncrementalADTree)
            self._data = self._data[self._mcvindex]
        elif isinstance(self._data,list):
            self._data = tuple(self._data)


    def _have_compact_children(self):
        return self._mcvindex is not None and isinstance(self._data,_IncrementalADTree)

    def _update(self, numvals, records, rmin, depth=0):
        """Add data to an L{_ADTree} object

        @param numvals: The ith element of this list is the number of values
        for the ith variable in the tree
        @type numvals: list
        @param records: The data as a list. Each element is a tuple of value
        indices, one for each variable, plus an extra count field as the final
        element.
        @type records: list
        @param depth: The depth of this tree within its containing
        CompactFactor. Also the index of the variable associated with the top
        node of this tree
        @type depth: int
        @param rmin: If the number of records is below C{rmin} then tree
        growing stops and C{records} is stored.  Note that a single record may
        represent many datapoints since all records have an extra 'count'
        field.
        @type rmin: int
        """

        for record in records:
            assert len(record)-1 == len(numvals)
            assert isinstance(record,tuple)
            for element in record:
                assert isinstance(element,int)

        # how many more?
        new_count = 0
        # each record has an extra 'count' field at the end
        for record in records:
            # all counts *must* be non-negative
            new_count += record[-1]

        # clearly if there are no new observations, there is no update.
        if new_count == 0:
            return

        self._count += new_count

        if depth == len(numvals):
            # no more variables to process
            self._data = []
            self._mcvindex = None
            return

        if self._mcvindex is None:
            # a bunch of records or empty
            merged_records = _merge_records(records, self._data, operator.add)

            # (XXX: repeated self._count calculation?)
            self._make_new_node(numvals, merged_records, rmin, depth)
            return

        # distribute records (XXX: hmm, calculate counts twice as above!)
        children_records, counts = _distribute_records(numvals[depth], depth, records)
        counts[self._mcvindex] = new_count
        children_records[self._mcvindex] = records

        # make self._data into a mutable list...
        if self._have_compact_children():
            # check to see if more than one non-empty subtree
            # (which must be expanded)
            for i in xrange(len(counts)):
                if i == self._mcvindex:
                    continue
                if counts[i] > 0:
                    other_than_mcv = True
                    break
            else:
                other_than_mcv = False

            # if not, then don't expand -- just update
            if not other_than_mcv:
                self._data._update(numvals,records,rmin,depth+1)
                return

            # else expand into full child set
            data = [None] * len(counts)
            data[self._mcvindex] = self._data
            self._data = data
            data = None
        else:
            self._data = list(self._data)

        # self._data is an instance of tuple -- update each branch
        for i, child_records in enumerate(children_records):
            child = self._data[i]
            if counts[i] == 0:
                continue

            if child is not None:
                child._update(numvals, child_records, rmin, depth+1)
            else:
                self._data[i] = _IncrementalADTree(numvals, child_records, rmin, depth+1)

        self._compact_children()
        if self._have_compact_children():
            assert self._count == self._data._count
        else:
            assert self._count == self._data[self._mcvindex]._count

        # the above update may result in the most common value no longer
        # being the most common value
        self._correct_mcv(numvals, rmin, depth)
        self._verify(numvals,rmin,depth)

    def _make_new_node(self,numvals,records,rmin,depth, force_expand=False):
        """Turn C{self} into a fresh node from records

        @param numvals: The ith element of this list is the number of values
        for the ith variable in the tree
        @type numvals: list
        @param records: The data as a list. Each element is a tuple of value
        indices, one for each variable, plus an extra count field as the final
        element.
        @type records: list
        @param depth: The depth of this tree within its containing
        CompactFactor. Also the index of the variable associated with the top
        node of this tree
        @type depth: int
        @param rmin: If the number of records is below C{rmin} then tree growing stops and C{records} is stored.
        Note that a single record may represent many datapoints since all records have an extra 'count' field.
        @type rmin: int
        """

        for record in records:
            assert len(record)-1 == len(numvals)
            assert isinstance(record,tuple)
            for element in record:
                assert isinstance(element,int)

        # all nodes have a count
        self._count = 0
        # each record has an extra 'count' field at the end
        for record in records:
            self._count += record[-1]

        if depth == len(numvals):
            # no more variables to process
            self._data = []
            self._mcvindex = None
            return

        if not force_expand and len(records) < rmin:
            # Case 2) above
            self._data = records
            self._mcvindex = None
            return

        children_records, counts = _distribute_records(numvals[depth], depth, records)

        # find most common value
        mcvcount = -1
        for valindex, count in enumerate(counts):
            if count > mcvcount:
                self._mcvindex, mcvcount = valindex, count
        # mcv branch actually has all records
        # not just those for which first=mcv
        children_records[self._mcvindex] = records
        counts[self._mcvindex] = self._count

        # make branches
        self._data = []
        other_than_mcv = False

        for i, child_records in enumerate(children_records):
            if counts[i] == 0:
                self._data.append(None)
                continue

            other_than_mcv |= i != self._mcvindex
            self._data.append(_IncrementalADTree(numvals,child_records, rmin, depth+1))

        self._compact_children(other_than_mcv)
        if self._have_compact_children():
            assert self._count == self._data._count
        else:
            assert self._count == self._data[self._mcvindex]._count
        self._verify(numvals,rmin,depth)

    def _expand_one_level(self, numvals, rmin, depth):
        """self is a record carrying node but we want to treat it like
        something bigger. Expand just this level. This is a special
        case of _make_new_node."""
        assert self._mcvindex is None

        self._make_new_node(numvals, self._data, rmin, depth, force_expand=True)

    def _correct_mcv(self,numvals,rmin,depth):
        if self._mcvindex is None:
            return

        if self._have_compact_children():
            self._data._correct_mcv(numvals,rmin,depth+1)
            return

        assert self._count == self._data[self._mcvindex]._count

        # calculate sum of all non-mcv values and find the most frequent of these values
        cur_mcv_count = self._count
        max_mcv = self._mcvindex
        max_mcv_count = 0
        for i, child in enumerate(self._data):
            if child is None or i == self._mcvindex:
                continue
            cur_mcv_count -= child._count
            if child._count > max_mcv_count:
                max_mcv, max_mcv_count = i, child._count

        # check if the current mcv is the true mcv...
        if cur_mcv_count < max_mcv_count:
            # if not, then recalculate the tree. generate
            # the tree of the wrong mcv, and move the vary node
            # stored at the wrong mcv to the correct mcv.
            # NOTE: this differs from the paper as the paper has
            # a mistake in it (wrong mcv and new mcv are the wrong way
            # around).
            data = list(self._data)
            data[self._mcvindex] = self._build_mcv(numvals, rmin, depth+1)
            assert (cur_mcv_count == 0 and data[self._mcvindex] is None) or data[self._mcvindex]._count == cur_mcv_count
            data[max_mcv] = self._data[self._mcvindex]
            self._data = data
            data = None
            self._mcvindex = max_mcv
            assert self._count == self._data[self._mcvindex]._count

            self._compact_children(cur_mcv_count != 0 or self._count > max_mcv_count)
            if self._have_compact_children():
                self._data._correct_mcv(numvals,rmin,depth+1)
                return

        # correct all the subtrees and other vary nodes
        for child in self._data:
            if child is not None:
                child._correct_mcv(numvals, rmin, depth+1)

    def _build_mcv(self, numvals, rmin, depth):
        assert self._mcvindex is not None

        # calculate the count of the mcv -- this is done before
        # the potentially more expensive full mcv calculation in case
        # it is zero, and for the case when there are no other variables
        # to consider.
        mcv_count = self._count
        if self._have_compact_children():
            vary_child = self._data
            children = []
        else:
            for i, child in enumerate(self._data):
                if child is None or i == self._mcvindex:
                    continue
                mcv_count -= child._count
            vary_child = self._data[self._mcvindex]
            children = [child for i, child in enumerate(self._data) if i != self._mcvindex]

        if mcv_count == 0:
            return None

        assert mcv_count > 0

        # end of the line (no more variables)?
        if depth == len(numvals):
            mcv = _IncrementalADTree(numvals, [], 1, depth)
            mcv._count = mcv_count
            return mcv

        # calculate the full tree of the most common value
        mcv = vary_child.copy()
        for child in children:
            mcv._subtract(numvals,rmin,depth,child)

        # sanity check
        assert mcv._count == mcv_count

        return mcv

    def _subtract(self, numvals, rmin, depth, lesser):

        if lesser is None:
            return

        # counts must be non-negative
        assert self._count >= lesser._count

        # expand record nodes if one of the nodes is not a record node
        if self._mcvindex is None and lesser._mcvindex is not None:
            self._expand_one_level(numvals, rmin, depth)

        # perform the subtraction
        self._count -= lesser._count

        # termination criterion
        if depth == len(numvals):
            return

        # deal with record nodes
        if self._mcvindex is None and lesser._mcvindex is None:
            self._data = _merge_records(self._data, lesser._data, operator.sub)
            return

        # expand record nodes if one of the nodes is not a record node
        if lesser._mcvindex is None:
            # use a copy -- that way we don't make lesser any
            # larger
            lesser = lesser.copy()
            lesser._expand_one_level(numvals, rmin, depth)

        self_single = not isinstance(self._data,tuple)
        less_single = not isinstance(lesser._data,tuple)

        # self must be larger than lesser
        if self_single and not less_single:
            # see if we can make less single by correcting
            # the mcv
            lesser._correct_mcv(numvals, rmin, depth)
            less_single = not isinstance(lesser._data,tuple)

        assert not self_single or less_single

        if self_single and less_single:
            assert self._mcvindex == lesser._mcvindex
            self_vary = self._data
            less_vary = lesser._data
            assert self_vary is None or self_vary._count == self._count + lesser._count
            assert less_vary is None or less_vary._count == lesser._count
        elif less_single:
            self_vary = self._data[self._mcvindex]
            less_vary = lesser._data
            assert self_vary is None or self_vary._count == self._count + lesser._count
            assert less_vary is None or less_vary._count == lesser._count
            if self._mcvindex != lesser._mcvindex:
                b = lesser._build_mcv(numvals, rmin, depth+1)
                if b is not None or b._count != 0:
                    assert self._data[lesser._mcvindex] is not None
                    assert self._data[lesser._mcvindex]._count != 0
                    if self._data[lesser._mcvindex]._count == b._count:
                        self._data = list(self._data)
                        self._data[lesser._mcvindex] = None
                        self._compact_children()
                    else:
                        self._data[lesser._mcvindex]._subtract(numvals,rmin, depth+1,b)
        else:
            self_vary = self._data[self._mcvindex]
            less_vary = lesser._data[lesser._mcvindex]
            assert self_vary is None or self_vary._count == self._count + lesser._count
            assert less_vary is None or less_vary._count == lesser._count
            # subtract the children
            self._data = list(self._data)
            for i, (a, b) in enumerate(zip(self._data, lesser._data)):
                if i == self._mcvindex:
                    continue

                if i == lesser._mcvindex:
                    b = lesser._build_mcv(numvals, rmin, depth+1)

                if b is None or b._count == 0:
                    continue

                assert a is not None and a._count != 0
                if a._count == b._count:
                    self._data[i] = None
                else:
                    a._subtract(numvals, rmin, depth+1, b)

            self._compact_children()

        # check vary node count consistency...
        assert self_vary is None or self_vary._count == self._count + lesser._count
        assert less_vary is None or less_vary._count == lesser._count

        if self_vary is not None:
            self_vary._subtract(numvals, rmin, depth+1, less_vary)
        else:
            assert less_vary._count == 0

        # check vary node count consistency...
        assert self_vary is None or self_vary._count == self._count
        assert less_vary is None or less_vary._count == lesser._count

    def _verify(self, numvals, rmin, depth=0):
        return
# slow running code
#S        """Ensure that the tree is in a consistent state. This imposes a set of
#S        structurally necessary (but not sufficient) conditions for a correct
#S        result."""
#S        if not __debug__:
#S            return
#S
#S        # an adtree represents a non-negative conditional contingency table
#S        assert self._count >= 0
#S
#S        # only the root node may have a zero count
#S        assert self._count != 0 or depth == 0
#S
#S        # if all at depth equal to the number of variables
#S        if depth == len(numvals):
#S            # it had better be that there is no mcv (since this must be a leaf
#S            # node)
#S            assert self._mcvindex is None
#S            # and there must be no associated data.
#S            assert self._data == []
#S            return
#S
#S        # if this node counts less than rmin instances
#S        if self._mcvindex is None:
#S            # and data must be a list of tuples...
#S            assert isinstance(self._data,list)
#S            # containing some count data (or this must be a zero count)...
#S            assert len(self._data) > 0 or self._count == 0
#S            # and these data must be tuples whose last
#S            # element is a non-negative integer
#S            s = 0
#S            for d in self._data:
#S                assert isinstance(d,tuple)
#S                assert d[-1] >= 0
#S                s += d[-1]
#S            # ensure correct count
#S            assert self._count == s
#S            return
#S
#S        # a non-leaf node had better have something interesting in it...
#S        assert self._count > 0
#S
#S        # is this a singleton internal node?
#S        if not isinstance(self._data, tuple):
#S            # better be an ad tree ... 
#S            assert isinstance(self._data, _IncrementalADTree)
#S            vary_child = self._data
#S        else:
#S            # otherwise, this had better have a valid most common value
#S            assert self._mcvindex >= 0 and self._mcvindex < numvals[depth]
#S            # and enough children
#S            assert isinstance(self._data,tuple)
#S            assert len(self._data) == numvals[depth]
#S            # the sum of every non-mcv child must be less than the count of
#S            # this node
#S            children = [child for i, child in enumerate(self._data)
#S                            if i != self._mcvindex and child is not None]
#S            children_counts = [child._count for child in children]
#S            assert sum(children_counts) <= self._count
#S            # there must exists one element other than the mcv that is not None
#S            assert len(children) > 0
#S            mcv_count = self._count
#S            # verify the non-mcv children
#S            for child in children:
#S                child._verify(numvals, rmin, depth+1)
#S                mcv_count -= child._count
#S            assert mcv_count >= max(children_counts)
#S            vary_child = self._data[self._mcvindex]
#S
#S        # the vary child (stored at the index of the most common value) must
#S        # have the same count as this node
#S        if vary_child is not None:
#S            assert self._count == vary_child._count
#S        else:
#S            assert self._count == 0
#S        # verify the vary child
#S        vary_child._verify(numvals, rmin, depth+1)


class IncrementalCompactFactor(SubDomain):
    
    def __init__(self,rawdata,domain=None,rmin=200):
        new_domain_variables, variables, records = rawdata[1:]
        SubDomain.__init__(self,variables,domain,new_domain_variables,check=True)
        # changing rmin between construction and _update calls on incremental
        # adtrees only has an effect where some other change in the tree ocurrs (i.e.,
        # where there is new data). for lack of confusion, store it here.
        self._rmin = rmin
        var_index = {}
        numvals = []
        for i, variable in enumerate(variables):
            var_index[variable] = i
            numvals.append(self._numvals[variable])
        # list of number of values, ordered according to self._var_index
        self._varnumvals = numvals
        self._tree = _IncrementalADTree(numvals,records,self._rmin)
        self._var_index = var_index
        self._tree._verify(self._varnumvals, self._rmin)


    def copy(self):
        cpy = copy(self)
        cpy._tree = cpy._tree.copy()
        return cpy

    def __getitem__(self,variables):
        return self.makeFactor(variables,check=True)

    def update(self, rawdata):
        records = rawdata[3]
        self._tree._update(self._varnumvals, records, self._rmin)
        self._tree._verify(self._varnumvals, self._rmin)

    def __str__(self):
        return "Variables: %s\nTree:\n%s\n" % (
            sorted(self._variables), self._tree.str(self._varnumvals, self._rmin))

    def makeFactor(self,variables,check=False):
        """Return a marginal factor with C{variables}
        
        Unless C{variables} is empty in which case return
        the sum of C{self}'s data
        @param variables: Variables to project onto
        @type variables: Iterable
        @param check: Whether to bother with an initial check that every member
        of C{variables} is in the L{CompactFactor}. (If this check is omitted
        and there I{is} an extra variable, then a L{Factor} with the wrong
        number of values will be created.)
        @type check: Boolean
        @return: The marginal factor
        @rtype: L{Factor} object
        @raise KeyError: if C{variables} contains a variable not contained in the
        L{CompactFactor}.
        """
        variables_info = [
            [self._var_index[var],self._numvals[var]]
            for var in variables]
        variables_info.sort()
        #size is the length of the list under each value of the corresponding variable
        size = 1
        for variable_info in reversed(variables_info):
            variable_info.append(size)
            size *= variable_info[1]
        return Factor(variables,self._tree._flatten(variables_info,0),SubDomain.copy(self))

    def makeCPT(self, child, parents, force_cpt=False, check=False, prior=0):
        """
        @param prior: the Dirichlet prior parameter (the same parameter value
        is used for all instances!)  Note there may be some problems with
        this method: a B{different} prior is used by the BDeu score. However,
        in practice, for parameter estimation, this prior method seems to be ok.
        I was lazy and it was simple to implement (cb).  If prior is zero, then
        the parameters are the maximum likelihood estimation solutions.
        """
        family = set(parents) | set([child])
        f_child = self.makeFactor(family)
        return CPT(f_child+prior, child, cpt_check=check, cpt_force=force_cpt)

    def family_score(self, child, parents, precision=1.0):
        return self.makeCPT(child, parents, force_cpt=False, check=False).bdeu_score(precision)

    def size(self):
        """Return the number of nodes in the underlying ADTree

        @return: The number of nodes in the tree
        @rtype: int
        """
        return self._tree._count

# try:
#     import psyco

#     psyco.bind(_merge_records)
#     psyco.bind(_distribute_records)
#     psyco.bind(_IncrementalADTree)
#     psyco.bind(IncrementalCompactFactor)
# except:
#     pass


#     def create_view(self,variables):
#         if variables:
#             cols = ','.join(sorted(variables))
#             sql = ('SELECT %s, sum(value) FROM %s GROUP BY %s' %
#                    (cols,self.table,cols))
#         else:
#             sql = 'SELECT sum(value) FROM %s' % self.table
#         self.cursor.execute('CREATE TEMP VIEW IF NOT EXISTS view_%s AS %s' % (cols,sql))
#         self._cached.append(frozenset(variables))

    # problem computing each view separately is inefficient since it involves many passes
    # and we only need one!
    # each pass is easier though.
    # if we find a high-dimensional database with few rows, then no need to store
    # lower dimensional ones.

    # given a frozenset of variables see if the exact table is stored
    # else for v in variables get set of all frozensets containing that variable
    # intersect them all, then use smallest of survivors

#         from math import log
#         x = frozenset(x)
#         y = frozenset(y)
#         #if x & y:
#         #    raise ValueError('%s and %s are not disjoint' % (x,y))

#         # construct 3 marginal tables
#         tables = (self.table,'temp0','temp0')
#         valcol = ('value','value0','value0')
#         for i, vs in enumerate(((x|y),x,y)):
#             vs = sorted(vs)
#             cols = ','.join([v + ' INT' for v in vs]+['value%d INT' % i])
#             sql = 'CREATE TABLE temp%d (%s)' % (i,cols)
#             self.cursor.execute(sql)
#             vs = ','.join(vs)
#             sql =  ('INSERT INTO temp%d ( %s, value%d ) SELECT %s, sum(%s) FROM %s GROUP BY %s' %
#                     (i,vs,i,vs,valcol[i],tables[i],vs))
#             #print sql
#             self.cursor.execute(sql)
#         # compute total count
#         self.cursor.execute('SELECT sum(value2) FROM temp2')
#         n = float(self.cursor.fetchone()[0])

#         # print 'xy'
# #         self.cursor.execute('SELECT * FROM temp0')
# #         print self.cursor.fetchall()

# #         print 'x'
# #         self.cursor.execute('SELECT * FROM temp1')
# #         print self.cursor.fetchall()

# #         print 'y'
# #         self.cursor.execute('SELECT * FROM temp2')
# #         print self.cursor.fetchall()

#         # join tables
#         self.cursor.execute('SELECT value0, value1, value2 FROM (temp0 NATURAL JOIN temp1) NATURAL JOIN temp2')
#         mi = 0.0
#         for row in self.cursor:
#             [nxy,nx,ny] = row
#             #print [n,nxy,nx,ny]
#             mi += (nxy/n) * log((n*nxy)/float(nx*ny))
#         for i in range(3):
#             self.cursor.execute('DROP TABLE temp%d' % i)
#         return mi
