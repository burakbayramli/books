"""Utility functions and variables

@var emptyset: The empty set
@type emptyset: Frozenset
@var _version: Version of this module
@type _version: String
"""

import Tkinter
from math import log, exp
import signal, time, sys, gc

_version = '$Id: Utils.py,v 1.3 2008/10/07 09:14:36 jc Exp $'

def neg_cnf(clauses):
    """Each clause is an iterable of integers

    Given a set of clauses, return a CNF asserting that not all are true.
    """

    # find suitable number for temporary z atoms

    first_z = 0
    for clause in clauses:
        for lit in clause:
            if abs(lit) > first_z:
                first_z = abs(lit)
    first_z += 1
    z = first_z

    # set up extra clauses with z literals
    
    z_clauses = {}
    disj = set()
    for clause in sorted(clauses,key=len):
        z_clauses[z] = []
        for lit in clause:
            z_clauses[z].append(-lit) # represents -z v -lit
        disj.add(z)
        z += 1
    ub_z = z
    out_clauses = set([frozenset(disj)])

    # eliminate all zs by resolution
    
    for z in range(first_z,ub_z):
        zset = frozenset([z])
        next_clauses = set()
        for lit in z_clauses[z]:
            litset = frozenset([lit])
            for clause in out_clauses:
                 if -lit not in clause: #only add new clause if not a tautology
                    next_clauses.add(clause - zset | litset )
        out_clauses = next_clauses

    # only check now for redundant clauses, could do this when eliminating zs.

    return_clauses = []
    for clause in out_clauses:
        for other in out_clauses:
            if other != clause and other <= clause:
                break
        else:
            return_clauses.append(clause)

    return return_clauses


def order_from_nonlex(variables,nonlexs):
    """Return C{variables} in standard order,
    except for those pairs of variables in C{nonlexs}

    @raise ValueError: If C{variables} and C{nonlexs} are inconsistent
    For example C{variables}=('a','b','c') and C{nonlexs} = [['a','c']]
    """
    nonlexs = frozenset([tuple(sorted(pair)) for pair in nonlexs])
    def compare(x,y):
        if x < y:
            if (x,y) in nonlexs:
                return 1
            else:
                return -1
        elif x > y:
            if (y,x) in nonlexs:
                return -1
            else:
                return 1
        else:
            return 0
    #print 'nonlexs', nonlexs
    output = sorted(variables,cmp=compare)
    
    # check that a consistent set of nonlexs was sent in
    for i, v in enumerate(output):
        for w in output[i+1:]:
            if (v < w and (v,w) in nonlexs) or (v > w and (w,v) not in nonlexs):
                raise ValueError('variables = %s and nonlexs = %s is inconsistent' % (variables,nonlexs))
    return output
                
                
    

def pretty_str_set(set):
    """Return pretty string representation of a set

    @param set: A set
    @type set: Set or Frozenset
    @return: Pretty string representation of a set
    @rtype: String
    """
    strs = [str(x) for x in set]
    return '{%s}' % ', '.join(tuple(sorted(strs)))

def member(some_set):
    """Return an arbitrary element from a set without altering the set.

    Can be used on any iterable. For ordered iterables returns the first
    member.
    @param some_set: A set from which an arbitrary member is required
    @type some_set: Set
    @return: Arbitrary element from C{some_set} or C{None} if the set is
    empty
    @rtype: Any
    """
    for x in some_set:
        return x

# def subsetn(set,n):
#     """Generate subsets of size at most C{n}

#     Actually C{set} can be any iterable and tuples
#     are returned. Each tuple is in ascending order

#     @param set: A 'set' of objects
#     @type set: Any iterable
#     @param n: Inclusive upper bound on size of subsets
#     @type n: Int
#     """
#     if n == 0:
#         yield ()
#     elif n == 1:
#         yield ()
#         for elt in set:
#             yield (elt,)
#     else:
#         m = n-1
#         for tup1 in subsetn(set,m):
#             yield tup1
#             if len(tup1) == m:
#                 for elt in set:
#                     if elt > tup1[-1]:
#                         yield tup1 + (elt,)


def all_subsets(set_in):
    """Generate all subsets of C{set_in} in no
    particular order.

    'Sets' are represented as lists
    Includes the emptyset and C{set_in}

    @param set_in: 'Set' for which subsets are sought
    @type set_in: List
    """
    if not set_in:
        yield set_in
    else:
        elt = set_in.pop()
        for x in all_subsets(set_in):
            yield x
            yield x + [elt]


def subsets_ascending(set_in,lim=None):
    """Generate all subsets of C{set_in}
    of size no more than C{lim} so that smaller subsets
    are always generated before bigger ones.

    Generated subsets are frozensets.
    Implementation: each layer generated separately.


    @param set_in: 'Set' for which subsets are sought
    @type set_in: Any iterable
    """
    if lim is None:
        lim = len(set_in)
    for i in range(lim+1):
        for subset in subseteqn(set_in,i):
            yield frozenset(subset)


def subsetn(set_in,n):
    """Generate subsets of size at most C{n}

    Actually C{set_in} can be any iterable.

    If n <= len(set_in)/2 tuples
    are returned. Each tuple is in ascending order. Otherwise
    a set is returned.

    This is optimised for n < len(set)/2

    @param set_in: A 'set' of objects
    @type set_in: Any iterable
    @param n: Inclusive upper bound on size of subsets
    @type n: Int
    """
    tupleset = tuple(sorted(set_in))
    if n == len(set_in):
        yield set_in
    elif n > len(set_in)/2:
        realset = set(set_in)
        for x in _subsetn(tupleset,len(set_in)-n):
            yield realset.difference(x[0])
    else:
        yield ()
        if n > 0:
            for x in _subsetn(tupleset,n):
                yield x[0]

def _subsetn(tupleset,n):
    if n == 1:
        for i, elt in enumerate(tupleset):
            yield (elt,), i
    else:
        n -= 1
        for tup1, lasti in _subsetn(tupleset,n):
            yield tup1, lasti
            if len(tup1) == n:
                lasti += 1
                for i, elt in enumerate(tupleset[lasti:]):
                    yield (tup1 + (elt,)), lasti + i


def subsetn_batch(set_in,n,batch_size):
    """
    Generate lists of size C{batch_size} containing subsets of size
    at most C{n} from C{set_in}.

    Final batch may be shorter than C{batch_size}
    """
    batch_template = [None] * batch_size
    batch = batch_template[:]
    i = 0
    for subset in subsetn(set_in,n):
        if i  == batch_size:
            yield batch
            batch = batch_template[:]
            i = 0
        batch[i] = subset
        i += 1
    yield batch[:i]


def subseteqn_batch(set_in,n,batch_size):
    """
    Generate lists of size C{batch_size} containing subsets of size
    C{n} from C{set_in}.

    Final batch may be shorter than C{batch_size}
    """
    batch_template = [None] * batch_size
    batch = batch_template[:]
    i = 0
    for subset in subseteqn(set_in,n):
        if i  == batch_size:
            yield batch
            batch = batch_template[:]
            i = 0
        batch[i] = subset
        i += 1
    yield batch[:i]

def subseteqn(set_in,n):
    """Generate subsets of size C{n}

    Actually C{set_in} can be any iterable.

    If n <= len(set_in)/2 tuples
    are returned. Each tuple is in ascending order. Otherwise
    a set is returned.

    This is optimised for n < len(set_in)/2

    @param set_in: A 'set' of objects
    @type set_in: Any iterable
    @param n: Inclusive upper bound on size of subsets
    @type n: Int
    """
    tupleset = tuple(sorted(set_in))
    if n > len(set_in)/2:
        realset = set(set_in)
        for x in _subseteqn(tupleset,len(set_in)-n):
            yield realset.difference(x[0])
    else:
        for x in _subseteqn(tupleset,n):
            yield x[0]

def _subseteqn(tupleset,n):
    if n == 0:
        yield (), None
    elif n == 1:
        for i, elt in enumerate(tupleset):
            yield (elt,), i
    else:
        m = n-1
        for tup1, lasti in _subseteqn(tupleset,m):
            if len(tup1) == m:
                lasti += 1
                for i, elt in enumerate(tupleset[lasti:]):
                    yield (tup1 + (elt,)), lasti + i



emptyset = frozenset()

class _ScrolledCanvas(Tkinter.Frame):
    def __init__(self,parent=Tkinter.NONE,width=1000,height=800,xscroll=10000,yscroll=1000):
        Tkinter.Frame.__init__(self, parent)
        top=self.winfo_toplevel() 
        top.rowconfigure(0, weight=1) 
        top.columnconfigure(0, weight=1) 
        self.rowconfigure(0, weight=1)
        self.columnconfigure(0, weight=1)
        canvframe = Tkinter.Frame(self)
        canvframe.rowconfigure(0, weight=1)
        canvframe.columnconfigure(0, weight=1)
        canv = Tkinter.Canvas(canvframe)
        # 
        canv.config(width=width,height=height)
        canv.config(scrollregion=(0,0,xscroll,yscroll))

        ysbar = Tkinter.Scrollbar(self)
        ysbar.config(command=canv.yview)
        canv.config(yscrollcommand=ysbar.set)

        xsbar = Tkinter.Scrollbar(self,orient=Tkinter.HORIZONTAL)
        xsbar.config(command=canv.xview)
        canv.config(xscrollcommand=xsbar.set)

        canv.grid(sticky=Tkinter.NSEW)
        canvframe.grid(row=0,column=0,sticky=Tkinter.NSEW)
        xsbar.grid(row=1,column=0,sticky=Tkinter.EW)
        ysbar.grid(row=0,column=1,sticky=Tkinter.NS)

        self.canv = canv
        self.canvframe = canvframe

def scrolled_frame(parent,width=1000,height=800,xscroll=10000,yscroll=1000):
    """Return a frame with scroll bars

    @param parent: The parent for the scrolled frame
    @type parent: Suitable TKinter object
    @param width: Width of frame
    @type width: Int
    @param height: Height of frame
    @type height: Int
    @param xscroll: Width of scrollable region
    @type xscroll: Int
    @param yscroll: Height of scrollable region
    @type yscroll: Int
    @return: Scrolled frame
    @rtype: C{Tkinter.Frame}
    """
    scr = _ScrolledCanvas(parent,width,height,xscroll,yscroll)
    gui =  Tkinter.Frame(scr.canvframe)
    scr.canv.create_window(0,0,window=gui,anchor=Tkinter.NW)
    scr.grid(sticky=Tkinter.NSEW)
    return gui

def logsumexp(xs):
    if not xs:
        raise RuntimeError,'logsumexp called with size 0 argument. (BUG AHOY!)'

    # numerical stability hack (based on Tom Minka's logsumexp.m)
    mx = max(xs)
    r = mx + log(sum([exp(x - mx) for x in xs]))
    return r

# in case underlying C implementation lacks these
try:
    infinity = float('infinity')
    negative_infinity = float('-infinity')
except ValueError:
    pass

def is_nan(x):
    return type(x) is float and x != x

def is_finite(x):
    # if infinity or negative_infinity not defined
    # then all floats are finite
    try:
        return not is_nan(x) and x not in (infinity,negative_infinity)
    except NameError:
        return True

def square(x):
    return x*x

def swap(x):
    return (x[1],x[0])

def rlog(x):
    """Restricted log; for x == 0 return - infinity; else log(x)
    Raises NameError if x is 0 and -infinity value not available
    """
    if x == 0:
        return negative_infinity
    return log(x)

#
# This file contains the Python code from Program 14.10 of
# "Data Structures and Algorithms
# with Object-Oriented Design Patterns in Python"
# by Bruno R. Preiss.
def binom(n, m):
    b = [0] * (n + 1)
    b[0] = 1
    for i in xrange(1, n + 1):
        b[i] = 1
        j = i - 1
        while j > 0:
            b[j] += b[j - 1]
            j -= 1
    return b[m]

# end of Bruno R. Preiss code.


def pairs(xs):
    xs = tuple(xs)
    for i, x in enumerate(xs):
        for y in xs[i+1:]:
            yield (x,y)
#     for i in xrange(len(xs)):
#         x = xs[i]
#         for y in xs[i+1:]:
#             yield (x,y)

def powerset(s):
    # {x | x <- 2^s }
    return _powerset(tuple(s))

def powersetn(s,n):
    # {x | x <- 2^s, |x| < n }
    if n is None or n >= len(s):
        return _powerset(tuple(s))
    return _powersetn(s,n)

def _powersetn(s,n):
    for i in xrange(0,n+1):
        for x in subsetn(s,i):
            yield x

def _powerset(s):
    # seems to be faster than using subsetn
    if not s:
        yield []
        return

    for x in ([],[s[0]]):
        for xs in _powerset(s[1:]):
            yield x + xs

class TimedOut(Exception):
    pass

class Timeout(object):
    """Timeouts. Usage:
    t = Timeout(30)
    try:
       try:
           t.start()
           ...
       except TimedOut:
           ...
    finally:
       t.clear()
    """
    def __init__(self, timeout):
        if timeout <= 0:
            raise RuntimeError, 'time out must be positive'
        super(Timeout, self).__init__()
        self._timeout = timeout
        self._oldhand = None

    def start(self):
        self._oldhand = signal.signal(signal.SIGALRM, self._ohno)
        signal.alarm(self._timeout)

    def clear(self):
        signal.alarm(0)
        if self._oldhand is not None:
            signal.signal(signal.SIGALRM, self._oldhand)
        self._oldhand = None

    def _ohno(self,signum,frame):
        # ensure one shot
        signal.signal(signal.SIGALRM, self._oldhand)
        self._oldhand = None
        signal.alarm(0)
        raise TimedOut


# from timeit.py
if sys.platform == "win32":
    # On Windows, the best timer is time.clock()
    default_timer = time.clock
else:
    # On most other platforms the best timer is time.time()
    default_timer = time.time

def tic(enable_gc=False):
    """Returns the current time in seconds.
    Typical usage:
        t = tic()
        ... do stuff n times ...
        total_time = toc(t,n)
    """
    if not enable_gc:
        gc.disable()
    return default_timer()

def toc(s,n,enable_gc=False):
    """Returns the average time elapsed in seconds.

    Typical usage:
        t = tic()
        ... do stuff n times ...
        total_time = toc(t,n)

    where n is the number of iterations of the process to be measured.
    """
    t = default_timer()
    if not enable_gc:
        gc.enable()
        gc.collect()
    return (t - s)/float(n)

