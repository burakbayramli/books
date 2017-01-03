from __future__ import division
from ch_07 import prim

from math import sqrt
from collections import defaultdict

def euc(a, b):
    return sqrt((a[0]-b[0])**2 + (a[1]-b[1])**2)

def euc_graph(pts):
    G = defaultdict(dict)
    for i, p in enumerate(pts):
        for j, q in enumerate(pts):
            if i == j: continue
            G[i][j] = euc(p,q)
    return G

def test_mtsp():
    """
    >>> G = euc_graph([
    ... (1,4), (1,2), (0,1), (3,4), (4,3), (3,2), (5,2), (2,0)
    ... ])
    >>> names = "abcdefgh"
    >>> [names[i] for i in mtsp(G, 0)] # Example from Cormen:
    ['a', 'b', 'c', 'h', 'd', 'e', 'f', 'g']
    """

from collections import defaultdict

def mtsp(G, r):                                 # 2-approx for metric TSP
    T, C = defaultdict(list), []                # Tree and cycle
    for c, p in prim(G, r).items():             # Build a traversable MSP
        T[p].append(c)                          # Child is parent's neighbor
    def walk(r):                                # Recursive DFS
        C.append(r)                             # Preorder node collection
        for v in T[r]: walk(v)                  # Visit subtrees recursively
    walk(r)                                     # Traverse from the root
    return C                                    # At least half-optimal cycle

from ch_08 import brutish_knapsack, rec_knapsack, knapsack

def test_knapsack():
    """
    >>> funcs = [brutish_knapsack, rec_knapsack, knapsack, bb_knapsack]
    >>> cases = [
    ...    #[[2, 4, 3, 6, 5], [2, 4, 3, 6, 6], 12, -1],
    ...    [[2, 3, 4, 5], [3, 4, 5, 6], 5, 7],
    ...    [[5, 1], [10, 75], 3, 75]
    ... ]
    >>> from random import *
    >>> for i in range(20):
    ...     n = randrange(10)
    ...     w = [randrange(1,100) for i in range(n)]
    ...     v = [randrange(1,100) for i in range(n)]
    ...     W = randrange(sum(w)+1)
    ...     cases.append([w, v, W, -1])
    >>> for w, v, W, e in cases:
    ...     sols = set(f(w, v, W) for f in funcs)
    ...     assert len(sols) == 1, (w, v, W, e, sols)
    ...     if e >= 0: assert sols.pop() == e
    ...
    >>>
    """

# Modified to run with 2.x (for the unit tests -- the 3.x version has also 
# been tested).

#from __future__ import division
from heapq import heappush, heappop
from itertools import count

def bb_knapsack(w, v, c):
    sol = [0]                                   # Solution so far
    n = len(w)                                  # Item count

    idxs = list(range(n))
    idxs.sort(key=lambda i: v[i]/w[i],          # Sort by descending unit cost
              reverse=True)

    def bound(sw, sv, m):                       # Greedy knapsack bound
        if m == n: return sv                    # No more items?
        objs = ((v[i], w[i]) for i in idxs[m:]) # Descending unit cost order
        for av, aw in objs:                     # Added value and weight
            if sw + aw > c: break               # Still room?
            sw += aw                            # Add wt to sum of wts
            sv += av                            # Add val to sum of vals
        return sv + (av/aw)*(c-sw)              # Add fraction of last item

    def node(sw, sv, m):                        # A node (generates children)
        #nonlocal sol                           # "Global" inside bb_knapsack
        if sw > c: return                       # Weight sum too large? Done
        sol[0] = max(sol[0], sv)                # Otherwise: Update solution
        if m == n: return                       # No more objects? Return
        i = idxs[m]                             # Get the right index
        ch = [(sw, sv), (sw+w[i], sv+v[i])]     # Children: without/with m
        for sw, sv in ch:                       # Try both possibilities
            b = bound(sw, sv, m+1)              # Bound for m+1 items
            if b > sol[0]:                      # Is the branch promising?
                yield b, node(sw, sv, m+1)      # Yield child w/bound

    num = count()                               # Helps avoid heap collisions
    Q = [(0, next(num), node(0, 0, 0))]         # Start with just the root
    while Q:                                    # Any nodes left?
        _, _, r = heappop(Q)                    # Get one
        for b, u in r:                          # Expand it...
            heappush(Q, (b, next(num), u))      # ... and push the children

    return sol[0]                               # Return the solution
