__meta__ = type

# ----------------------------------------------------------------------------

from functools import wraps

def memo(func):
    cache = {}                                  # Stored subproblem solutions
    @wraps(func)                                # Make wrap look like func
    def wrap(*args):                            # The memoized wrapper
        if args not in cache:                   # Not already computed?
            cache[args] = func(*args)           # Compute & cache the solution
        return cache[args]                      # Return the cached solution
    return wrap                                 # Return the wrapper

def test_memo():
    """
    >>> @memo
    ... def fib(i):
    ...     if i < 2: return 1
    ...     return fib(i-1) + fib(i-2)
    ... 
    >>> fib(10)
    89
    >>> #fib = memo(fib)
    >>> print(fib(100)) # Avoid the L suffix in 2.7
    573147844013817084101
    >>> @memo
    ... def two_pow(i):
    ...     if i == 0: return 1
    ...     return two_pow(i-1) + two_pow(i-1)
    ...
    >>> two_pow(10)
    1024
    >>> print(two_pow(100))
    1267650600228229401496703205376
    >>> def two_pow(i):
    ...     if i == 0: return 1
    ...     return 2*two_pow(i-1)
    ...
    >>> two_pow(10)
    1024
    >>> print(two_pow(100))
    1267650600228229401496703205376
    """

# ----------------------------------------------------------------------------

from itertools import combinations

def naive_lis(seq):
    for length in range(len(seq), 0, -1):       # n, n-1, ... , 1
        for sub in combinations(seq, length):   # Subsequences of given length
            if list(sub) == sorted(sub):        # An increasing subsequence?
                return sub                      # Return it!

def test_lis():
    """
    >>> seq = [3, 1, 0, 2, 4]
    >>> naive_lis(seq)
    (1, 2, 4)
    >>> rec_lis(seq)
    3
    >>> basic_lis(seq)
    3
    >>> lis(seq)
    3
    >>> naive_lis([1, 0, 7, 2, 8, 3, 4, 9, 5, 6])
    (1, 2, 3, 4, 5, 6)
    >>> from random import *
    >>> seqs = [[randrange(100) for i in range(5+i)] for i in range(10)]
    >>> seqs.append([1, 1, 1, 1, 2, 2, 2, 2, 0, 0, 0, 4, 3, 3, 3, 4, 4, 4])
    >>> for seq in seqs:
    ...     res = naive_lis(seq)
    ...     for f in [basic_lis, rec_lis, lis]:
    ...         res2 = f(seq)
    ...         assert res2 == len(res), (res, seq, res2, f)
    """

def rec_lis(seq):                               # Longest increasing subseq.
    @memo
    def L(cur):                                 # Longest ending at seq[cur]
        res = 1                                 # Length is at least 1
        for pre in range(cur):                  # Potential predecessors
            if seq[pre] <= seq[cur]:            # A valid (smaller) predec.
                res = max(res, 1 + L(pre))      # Can we improve the solution?
        return res
    return max(L(i) for i in range(len(seq)))   # The longest of them all

def basic_lis(seq):
    L = [1] * len(seq)
    for cur, val in enumerate(seq):
        for pre in range(cur):
            if seq[pre] <= val:
                L[cur] = max(L[cur], 1 + L[pre])
    return max(L)

from bisect import bisect

def lis(seq):                                   # Longest increasing subseq.
    end = []                                    # End-values for all lengths
    for val in seq:                             # Try every value, in order
        idx = bisect(end, val)                  # Can we build on an end val?
        if idx == len(end): end.append(val)     # Longest seq. extended
        else: end[idx] = val                    # Prev. endpoint reduced
    return len(end)                             # The longest we found

DAG = {
    'a': {'b':0},
    'b': {'c':4, 'd':6},
    'c': {'g':2, 'h':-6},
    'd': {'f':3, 'e':5},
    'e': {'g':0, 'h':-6},
    'f': {'i':-1},
    'g': {'h':4},
    'h': {'i':7},
    'i': {}
}

def test_dag_sp():
    """
    >>> rec_dag_sp(DAG, 'a', 'i')
    5
    >>> dag_sp(DAG, 'a', 'i')
    5
    """

def rec_dag_sp(W, s, t):                        # Shortest path from s to t
    @memo                                       # Memoize f
    def d(u):                                   # Distance from u to t
        if u == t: return 0                     # We're there!
        return min(W[u][v]+d(v) for v in W[u])  # Best of every first step
    return d(s)                                 # Apply f to actual start node

# From Chapter 4:
def topsort(G):
    count = dict((u, 0) for u in G)             # The in-degree for each node
    for u in G:
        for v in G[u]:
            count[v] += 1                       # Count every in-edge
    Q = [u for u in G if count[u] == 0]         # Valid initial nodes
    S = []                                      # The result
    while Q:                                    # While we have start nodes...
        u = Q.pop()                             # Pick one
        S.append(u)                             # Use it as first of the rest
        for v in G[u]:
            count[v] -= 1                       # "Uncount" its out-edges
            if count[v] == 0:                   # New valid start nodes?
                Q.append(v)                     # Deal with them next
    return S

def dag_sp(W, s, t):                            # Shortest path from s to t
    d = {u:float('inf') for u in W}             # Distance estimates
    d[s] = 0                                    # Start node: Zero distance
    for u in topsort(W):                        # In top-sorted order...
        if u == t: break                        # Have we arrived?
        for v in W[u]:                          # For each out-edge ...
            d[v] = min(d[v], d[u] + W[u][v])    # Relax the edge
    return d[t]                                 # Distance to t (from s)

# ----------------------------------------------------------------------------

def test_c():
    """
    >>> @memo
    ... def C(n,k):
    ...     if k == 0: return 1
    ...     if n == 0: return 0
    ...     return C(n-1,k-1) + C(n-1,k)
    >>> C(4,2)
    6
    >>> print(C(100,50))
    100891344545564193334812497256
    >>> C(10,7)
    120
    >>> C(4, 4)
    1
    >>> C(4, 5)
    0
    """

def test_c2():
    """
    >>> from collections import defaultdict
    >>> n, k = 10, 7
    >>> C = defaultdict(int)
    >>> for row in range(n+1):
    ...     C[row,0] = 1
    ...     for col in range(1,k+1):
    ...         C[row,col] = C[row-1,col-1] + C[row-1,col]
    ... 
    >>> C[n,k]
    120
    """

# ----------------------------------------------------------------------------

def test_lcs():
    """
    >>> rec_lcs("spock", "asoka")
    3
    >>> rec_lcs("AGCGA", "CAGATAGAG")
    4
    >>> rec_lcs("Starbuck", "Starwalker")
    5
    >>> lcs("spock", "asoka")
    3
    >>> lcs("AGCGA", "CAGATAGAG")
    4
    >>> lcs("Starbuck", "Starwalker")
    5
    """

def rec_lcs(a,b):                               # Longest common subsequence
    @memo                                       # L is memoized
    def L(i,j):                                 # Prefixes a[:i] and b[:j]
        if min(i,j) < 0: return 0               # One prefix is empty
        if a[i] == b[j]: return 1 + L(i-1,j-1)  # Match! Move diagonally
        return max(L(i-1,j), L(i,j-1))          # Chop off either a[i] or b[j]
    return L(len(a)-1,len(b)-1)                 # Run L on entire sequences

def lcs(a,b):
    n, m = len(a), len(b)
    pre, cur = [0]*(n+1), [0]*(n+1)             # Previous/current row
    for j in range(1,m+1):                      # Iterate over b
        pre, cur = cur, pre                     # Keep prev., overwrite cur.
        for i in range(1,n+1):                  # Iterate over a
            if a[i-1] == b[j-1]:                # Last elts. of pref. equal?
                cur[i] = pre[i-1] + 1           # L(i,j) = L(i-1,j-1) + 1
            else:                               # Otherwise...
                cur[i] = max(pre[i], cur[i-1])  # max(L(i,j-1),L(i-1,j))
    return cur[n]                               # L(n,m)

# ----------------------------------------------------------------------------

def test_knapsack():
    """
    >>> funcs = [brutish_knapsack, old_rec_knapsack, rec_knapsack, 
    ... knapsack]
    >>> cases = [
    ...    #[[2, 4, 3, 6, 5], [2, 4, 3, 6, 6], 12, -1],
    ...    [[2, 3, 4, 5], [3, 4, 5, 6], 5, 7]
    ... ]
    >>> from random import *
    >>> for i in range(20):
    ...     n = randrange(10)
    ...     w = [randrange(100) for i in range(n)]
    ...     v = [randrange(100) for i in range(n)]
    ...     W = randrange(sum(w)+1)
    ...     cases.append([w, v, W, -1])
    >>> for w, v, W, e in cases:
    ...     sols = set(f(w, v, W) for f in funcs)
    ...     assert len(sols) == 1, (w, v, W, e, sols)
    ...     if e >= 0: assert sols.pop() == e
    ...
    >>>
    """

# Not used -- just for testing:
def brutish_knapsack(w, v, W):
    items = list(range(len(w)))
    vals = [0]
    for r in range(1,len(items)+1):
        for subset in combinations(items, r):
            wt = sum(w[x] for x in subset)
            if wt <= W: vals.append(sum(v[x] for x in subset))
    return max(vals)

def rec_knapsack(w, v, c):                      # Weights, values and capacity
    @memo                                       # m is memoized
    def m(k, r):                                # Max val., k objs and cap r
        if k == 0 or r == 0: return 0           # No objects/no capacity
        i = k-1                                 # Object under consideration
        drop = m(k-1, r)                        # What if we drop the object?
        if w[i] > r: return drop                # Too heavy: Must drop it
        return max(drop, v[i] + m(k-1, r-w[i])) # Include it? Max of in/out
    return m(len(w), c)                         # All objects, all capacity

def old_rec_knapsack(w, v, c):                  # Weights, values and capacity
    @memo                                       # m is memoized
    def m(i, r):                                # Max val., obj 0..i and cap r
        if i == -1 or r == 0: return 0          # No objects/no capacity
        drop = m(i-1, r)                        # What if we drop object i?
        if w[i] > r: return drop                # Too heavy: Must drop it
        return max(drop, v[i] + m(i-1, r-w[i])) # Include it? Max of in/out
    return m(len(w)-1, c)                       # All objects, all capacity

def knapsack_old(w, v, c):
    n = len(w)
    m = [[0]*(c+1) for i in range(n+1)]
    for k in range(1,n+1):
        i = k-1
        for r in range(1,c+1):
            m[k][r] = drop = m[k-1][r]
            if w[i] <= r:
                m[k][r] = max(drop, v[i] + m[k-1][r-w[i]])
    return m[n][c]

def knapsack_wrap(w, v, c):
    return knapsack_inner(w, v, c)[0][len(w)][c]

def test_knapsack_items():
    """
    >>> knapsack = knapsack_inner
    >>> w, v, c = [2, 3, 4, 5], [3, 4, 5, 6], 5
    >>> m, P = knapsack(w, v, c)
    >>> k, r, items = len(w), c, set()
    >>> while k > 0 and r > 0:
    ...     i = k-1
    ...     if P[k][r]:
    ...         items.add(i)
    ...         r -= w[i]
    ...     k -= 1
    ...
    >>> sorted(items)
    [0, 1]
    """

def knapsack(w, v, c):                          # Returns solution matrices
    n = len(w)                                  # Number of available items
    m = [[0]*(c+1) for i in range(n+1)]         # Empty max-value matrix
    P = [[False]*(c+1) for i in range(n+1)]     # Empty keep/drop matrix
    for k in range(1,n+1):                      # We can use k first objects
        i = k-1                                 # Object under consideration
        for r in range(1,c+1):                  # Every positive capacity
            m[k][r] = drop = m[k-1][r]          # By default: drop the object
            if w[i] > r: continue               # Too heavy? Ignore it
            keep = v[i] + m[k-1][r-w[i]]        # Value of keeping it
            m[k][r] = max(drop, keep)           # Best of dropping and keeping
            P[k][r] = keep > drop               # Did we keep it?
    return m, P                                 # Return full results

knapsack_inner = knapsack
knapsack = knapsack_wrap

def test_unbounded_knapsack():
    """
    >>> funcs = [rec_unbounded_knapsack, unbounded_knapsack]
    >>> w, v = [1, 2], [2, 5]
    >>> [f(w, v, 5) for f in funcs]
    [12, 12]
    >>> w, v = [3, 2, 4], [5, 4, 2]
    >>> [f(w, v, 7) for f in funcs]
    [13, 13]
    """

def rec_unbounded_knapsack(w, v, c):            # Weights, values and capacity
    @memo                                       # m is memoized
    def m(r):                                   # Max val. w/remaining cap. r
        if r == 0: return 0                     # No capacity? No value
        val = m(r-1)                            # Ignore the last cap. unit?
        for i, wi in enumerate(w):              # Try every object
            if wi > r: continue                 # Too heavy? Ignore it
            val = max(val, v[i] + m(r-wi))      # Add value, remove weight
        return val                              # Max over all last objects
    return m(c)                                 # Full capacity available

def unbounded_knapsack(w, v, c):
    m = [0]
    for r in range(1,c+1):
        val = m[r-1]
        for i, wi in enumerate(w):
            if wi > r: continue
            val = max(val, v[i] + m[r-wi])
        m.append(val)
    return m[c]

# ----------------------------------------------------------------------------

def test_opt_tree():
    """
    >>> w = [0.25, 0.2, 0.05, 0.2, 0.3]
    >>> rec_opt_tree(w)
    2.1
    >>> opt_tree(w)
    2.1
    >>> from random import *
    >>> ws = [[random() for i in range(randrange(4,9))] for j in range(20)]
    >>> for w in ws:
    ...     assert rec_opt_tree(w) == opt_tree(w)
    """

def rec_opt_tree(p):
    @memo
    def s(i,j):
        if i == j: return 0
        return s(i,j-1) + p[j-1]
    @memo
    def e(i,j):
        if i == j: return 0
        sub = min(e(i,r) + e(r+1,j) for r in range(i,j))
        return sub + s(i,j)
    return e(0,len(p))

from collections import defaultdict

def opt_tree(p):
    n = len(p)
    s, e = defaultdict(int), defaultdict(int)
    for l in range(1,n+1):
        for i in range(n-l+1):
            j = i + l
            s[i,j] = s[i,j-1] + p[j-1]
            e[i,j] = min(e[i,r] + e[r+1,j] for r in range(i,j))
            e[i,j] += s[i,j]
    return e[0,n]
