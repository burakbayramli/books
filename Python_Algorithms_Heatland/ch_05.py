def some_graph():
    a, b, c, d, e, f, g, h = range(8)
    N = [
        [b, c, d, e, f],    # a
        [c, e],             # b
        [d],                # c
        [e],                # d
        [f],                # e
        [c, g, h],          # f
        [f, h],             # g
        [f, g]              # h
    ]
    return N

def some_tree():
    a, b, c, d, e, f, g, h = range(8)
    N = [
        [b, c],             # a
        [d, e],             # b
        [f, g],             # c
        [],                 # d
        [],                 # e
        [],                 # f
        [h],                # g
        []                  # h
    ]
    return N

class stack(list):
    add = list.append

def test_traverse():
    """
    >>> G = some_graph()
    >>> list(traverse(G, 0))
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> list(traverse(G, 0, stack))
    [0, 5, 7, 6, 2, 3, 4, 1]
    >>> for i in range(len(G)): G[i] = set(G[i])
    >>> sorted(walk(G, 0))
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> G = {
    ...    0: set([1, 2]),
    ...    1: set([0, 2]),
    ...    2: set([0, 1]),
    ...    3: set([4, 5]),
    ...    4: set([3, 5]),
    ...    5: set([3, 4])
    ... }
    >>> comp = []
    >>> seen = set()
    >>> for u in G:
    ...     if u in seen: continue
    ...     C = walk(G, u)
    ...     seen.update(C)
    ...     comp.append(C)
    ...
    >>> [list(sorted(C)) for C in comp]
    [[0, 1, 2], [3, 4, 5]]
    >>> [list(sorted(C)) for C in components(G)]
    [[0, 1, 2], [3, 4, 5]]
    """

def walk(G, s, S=set()):                        # Walk the graph from node s
    P, Q = dict(), set()                        # Predecessors + "to do" queue
    P[s] = None                                 # s has no predecessor
    Q.add(s)                                    # We plan on starting with s
    while Q:                                    # Still nodes to visit
        u = Q.pop()                             # Pick one, arbitrarily
        for v in G[u].difference(P, S):         # New nodes?
            Q.add(v)                            # We plan to visit them!
            P[v] = u                            # Remember where we came from
    return P                                    # The traversal tree

def components(G):                              # The connected components
    comp = []
    seen = set()                                # Nodes we've already seen
    for u in G:                                 # Try every starting point
        if u in seen: continue                  # Seen? Ignore it
        C = walk(G, u)                          # Traverse component
        seen.update(C)                          # Add keys of C to seen
        comp.append(C)                          # Collect the components
    return comp

def traverse(G, s, qtype=set):
    S, Q = set(), qtype()
    Q.add(s)
    while Q:
        u = Q.pop()
        if u in S: continue
        S.add(u)
        for v in G[u]:
            Q.add(v)
        yield u

def test_tree_walk():
    """
    >>> T = some_tree()
    >>> tree_walk(T, 0) # Testing that it doesn't crash
    >>> list(tree_walk_tested(T, 0)) # Get the ordering
    [0, 1, 3, 4, 2, 5, 6, 7]
    """

def tree_walk(T, r):                            # Traverse T from root r
    for u in T[r]:                              # For each child...
        tree_walk(T, u)                         # ... traverse its subtree

def tree_walk_tested(T, r):
    yield r # For testing
    for u in T[r]:
        for v in tree_walk_tested(T, u):
            yield v

def test_dfs():
    """
    >>> G = some_graph()
    >>> for i in range(len(G)): G[i] = set(G[i])
    >>> list(rec_dfs(G, 0))
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> rec_dfs_tested(G, 0)
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> list(iter_dfs(G, 0))
    [0, 5, 7, 6, 2, 3, 4, 1]
    >>> d = {}; f = {}
    >>> dfs(G, 0, d, f)
    16
    >>> [d[v] for v in range(len(G))]
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> [f[v] for v in range(len(G))]
    [15, 14, 13, 12, 11, 10, 9, 8]
    """

# Important: Can't use "for u in G[s] - S" here, bc S might change
def rec_dfs(G, s, S=None):
    if S is None: S = set()                     # Initialize the history
    S.add(s)                                    # We've visited s
    for u in G[s]:                              # Explore neighbors
        if u in S: continue                     # Already visited: Skip
        rec_dfs(G, u, S)                        # New: Explore recursively
    return S # For testing

def rec_dfs_tested(G, s, S=None):
    if S is None: S = []
    S.append(s)
    for u in G[s]:
        if u in S: continue
        rec_dfs_tested(G, u, S)
    return S

def iter_dfs(G, s):
    S, Q = set(), []                            # Visited-set and queue
    Q.append(s)                                 # We plan on visiting s
    while Q:                                    # Planned nodes left?
        u = Q.pop()                             # Get one
        if u in S: continue                     # Already visited? Skip it
        S.add(u)                                # We've visited it now
        Q.extend(G[u])                          # Schedule all neighbors
        yield u                                 # Report u as visited

def dfs(G, s, d, f, S=None, t=0):
    if S is None: S = set()                     # Initialize the history
    d[s] = t; t += 1                            # Set discover time
    S.add(s)                                    # We've visited s
    for u in G[s]:                              # Explore neighbors
        if u in S: continue                     # Already visited. Skip
        t = dfs(G, u, d, f, S, t)               # Recurse; update timestamp
    f[s] = t; t += 1                            # Set finish time
    return t                                    # Return timestamp

def test_dfs_topsort():
    """
    >>> n = 6
    >>> from random import sample, randrange, shuffle
    >>> from random import seed; seed(2365)
    >>> G = dict()
    >>> seq = list(range(n)) # Py 3 range objects aren't sequences
    >>> shuffle(seq)
    >>> rest = set(seq)
    >>> for x in seq[:-1]:
    ...     rest.remove(x)
    ...     m = randrange(1,len(rest)+1)
    ...     G[x] = set(sample(rest, m))
    ...
    >>> G[seq[-1]] = set()
    >>> sorted = dfs_topsort(G)
    >>> rest = set(sorted)
    >>> for u in sorted:
    ...     rest.remove(u)
    ...     assert G[u] <= rest
    ...
    >>> G = {'a': set('bf'), 'b': set('cdf'),
    ... 'c': set('d'), 'd': set('ef'), 'e': set('f'), 'f': set()}
    >>> dfs_topsort(G)
    ['a', 'b', 'c', 'd', 'e', 'f']
    """

def dfs_topsort(G):
    S, res = set(), []                          # History and result
    def recurse(u):                             # Traversal subroutine
        if u in S: return                       # Ignore visited nodes
        S.add(u)                                # Otherwise: Add to history
        for v in G[u]:
            recurse(v)                          # Recurse through neighbors
        res.append(u)                           # Finished with u: Append it
    for u in G:
        recurse(u)                              # Cover entire graph
    res.reverse()                               # It's all backward so far
    return res

def test_iddfs_and_bfs():
    """
    >>> G = some_graph()
    >>> list(iddfs(G, 0))
    [0, 1, 2, 3, 4, 5, 6, 7]
    >>> bfs(G, 0)
    {0: None, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 5, 7: 5}
    >>> G = [[1, 2], [0, 3], [0, 3], [1, 2]]
    >>> list(iddfs(G, 0))
    [0, 1, 2, 3]
    >>> bfs(G, 0)
    {0: None, 1: 0, 2: 0, 3: 1}
    >>> P = _
    >>> u = 3
    >>> path = [u]
    >>> while P[u] is not None:
    ...     path.append(P[u])
    ...     u = P[u]
    ...
    >>> path.reverse()
    >>> path
    [0, 1, 3]
    """

def iddfs(G, s):
    yielded = set()                             # Visited for the first time
    def recurse(G, s, d, S=None):               # Depth-limited DFS
        if s not in yielded:
            yield s
            yielded.add(s)
        if d == 0: return                       # Max depth zero: Backtrack
        if S is None: S = set()
        S.add(s)
        for u in G[s]:
            if u in S: continue
            for v in recurse(G, u, d-1, S):     # Recurse with depth-1
                yield v
    n = len(G)
    for d in range(n):                          # Try all depths 0..V-1
        if len(yielded) == n: break             # All nodes seen?
        for u in recurse(G, s, d):
            yield u

from collections import deque

def bfs(G, s):
    P, Q = {s: None}, deque([s])                # Parents and FIFO queue
    while Q:
        u = Q.popleft()                         # Constant-time for deque
        for v in G[u]:
            if v in P: continue                 # Already has parent
            P[v] = u                            # Reached from u: u is parent
            Q.append(v)
    return P

from string import ascii_lowercase
def parse_graph(s):
    G = {}
    for u, line in zip(ascii_lowercase, s.split("/")):
        G[u] = set(line)
    return G

def test_scc():
    """
    >>> G = parse_graph('bc/die/d/ah/f/g/eh/i/h')
    >>> list(map(list, scc(G)))
    [['a', 'c', 'b', 'd'], ['e', 'g', 'f'], ['i', 'h']]
    """

def tr(G):                                      # Transpose (rev. edges of) G
    GT = {}
    for u in G: GT[u] = set()                   # Get all the nodes in there
    for u in G:
        for v in G[u]:
            GT[v].add(u)                        # Add all reverse edges
    return GT

def scc(G):
    GT = tr(G)                                  # Get the transposed graph
    sccs, seen = [], set()
    for u in dfs_topsort(G):                    # DFS starting points
        if u in seen: continue                  # Ignore covered nodes
        C = walk(GT, u, seen)                   # Don't go "backward" (seen)
        seen.update(C)                          # We've now seen C
        sccs.append(C)                          # Another SCC found
    return sccs
