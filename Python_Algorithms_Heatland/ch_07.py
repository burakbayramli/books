
def greedy(E, S, w):
    T = []                                      # Emtpy, partial solution
    for e in sorted(E, key=w):                  # Greedily consider elements
        TT = T + [e]                            # Tentative solution
        if TT in S: T = TT                      # Is it valid? Use it!
    return T

def test_making_change():
    """
    >>> denom = [10000, 5000, 2000, 1000, 500, 200, 100, 50, 25, 10, 5, 1]
    >>> owed = 56329
    >>> payed = []
    >>> for d in denom:
    ...     while owed >= d:
    ...         owed -= d
    ...         payed.append(d)
    ...
    >>> sum(payed)
    56329
    >>> len(payed)
    14
    """

def test_huffman():
    """
    >>> seq = "abcdefghi"
    >>> frq = [4, 5, 6, 9, 11, 12, 15, 16, 20]
    >>> huffman(seq, frq)
    [['i', [['a', 'b'], 'e']], [['f', 'g'], [['c', 'd'], 'h']]]
    >>> C = dict(codes(_))
    >>> C['i'], C['a'], C['c']
    ('00', '0100', '1100')
    """

def codes(tree, prefix=""):
    if len(tree) == 1:
        yield (tree, prefix)                    # A leaf with its code
        return
    for bit, child in zip("01", tree):          # Left (0) and right (1)
        for pair in codes(child, prefix + bit): # Get codes recursively
            yield pair

from heapq import heapify, heappush, heappop
from itertools import count

def huffman(seq, frq):
    num = count()
    trees = list(zip(frq, num, seq))            # num ensures valid ordering
    heapify(trees)                              # A min-heap based on freq
    while len(trees) > 1:                       # Until all are combined
        fa, _, a = heappop(trees)               # Get the two smallest trees
        fb, _, b = heappop(trees)
        n = next(num)
        heappush(trees, (fa+fb, n, [a, b]))     # Combine and re-add them
    return trees[0][-1]

def test_naive_kruskal():
    """
    >>> G = {
    ...   0: {1:1, 2:3, 3:4},
    ...   1: {2:5},
    ...   2: {3:2},
    ...   3: set()
    ... }
    >>> list(naive_kruskal(G))
    [(0, 1), (2, 3), (0, 2)]
    """

def naive_find(C, u):                           # Find component rep.
    while C[u] != u:                            # Rep. would point to itself
        u = C[u]
    return u

def naive_union(C, u, v):
    u = naive_find(C, u)                        # Find both reps
    v = naive_find(C, v)
    C[u] = v                                    # Make one refer to the other

def naive_kruskal(G):
    E = [(G[u][v],u,v) for u in G for v in G[u]]
    T = set()                                   # Empty partial solution
    C = {u:u for u in G}                        # Component reps
    for _, u, v in sorted(E):                   # Edges, sorted by weight
        if naive_find(C, u) != naive_find(C, v):
            T.add((u, v))                       # Different reps? Use it!
            naive_union(C, u, v)                # Combine components
    return T

def test_kruskal():
    """
    >>> G = {
    ...   0: {1:1, 2:3, 3:4},
    ...   1: {2:5},
    ...   2: {3:2},
    ...   3: set()
    ... }
    >>> list(kruskal(G))
    [(0, 1), (2, 3), (0, 2)]
    """

def find(C, u):
    if C[u] != u:
        C[u] = find(C, C[u])                    # Path compression
    return C[u]

def union(C, R, u, v):
    u, v = find(C, u), find(C, v)
    if R[u] > R[v]:                             # Union by rank
        C[v] = u
    else:
        C[u] = v
    if R[u] == R[v]:                            # A tie: Move v up a level
        R[v] += 1

def kruskal(G):
    E = [(G[u][v],u,v) for u in G for v in G[u]]
    T = set()
    C, R = {u:u for u in G}, {u:0 for u in G}   # Comp. reps and ranks
    for _, u, v in sorted(E):
        if find(C, u) != find(C, v):
            T.add((u, v))
            union(C, R, u, v)
    return T

def test_prim():
    """
    >>> G = {
    ...   0: {1:1, 2:3, 3:4},
    ...   1: {0:1, 2:5},
    ...   2: {0:3, 1:5, 3:2},
    ...   3: {2:2, 0:4}
    ... }
    >>> prim(G, 0)
    {0: None, 1: 0, 2: 0, 3: 2}
    """

from heapq import heappop, heappush

def prim(G, s):
    P, Q = {}, [(0, None, s)]
    while Q:
        _, p, u = heappop(Q)
        if u in P: continue
        P[u] = p
        for v, w in G[u].items():
            heappush(Q, (w, u, v))
    return P
