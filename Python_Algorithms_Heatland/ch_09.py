def test_relax():
    """
    >>> u = 0; v = 1
    >>> D, W, P = {}, {u:{v:3}}, {}
    >>> D[u] = 7
    >>> D[v] = 13
    >>> D[u]
    7
    >>> D[v]
    13
    >>> W[u][v]
    3
    >>> relax(W, u, v, D, P)
    True
    >>> D[v]
    10
    >>> D[v] = 8
    >>> relax(W, u, v, D, P)
    >>> D[v]
    8
    """

inf = float('inf')
def relax(W, u, v, D, P):
    d = D.get(u,inf) + W[u][v]                  # Possible shortcut estimate
    if d < D.get(v,inf):                        # Is it really a shortcut?
        D[v], P[v] = d, u                       # Update estimate and parent
        return True                             # There was a change!

def test_bellman_ford():
    """
    >>> s, t, x, y, z = range(5)
    >>> W = {
    ...     s: {t:6, y:7},
    ...     t: {x:5, y:8, z:-4},
    ...     x: {t:-2},
    ...     y: {x:-3, z:9},
    ...     z: {s:2, x:7}
    ... }
    >>> D, P = bellman_ford(W, s)
    >>> [D[v] for v in [s, t, x, y, z]]
    [0, 2, 4, 7, -2]
    >>> s not in P
    True
    >>> [P[v] for v in [t, x, y, z]] == [x, y, s, t]
    True
    >>> W[s][t] = -100
    >>> bellman_ford(W, s)
    Traceback (most recent call last):
            ...
    ValueError: negative cycle
    """

def bellman_ford(G, s):
    D, P = {s:0}, {}                            # Zero-dist to s; no parents
    for rnd in G:                               # n = len(G) rounds
        changed = False                         # No changes in round so far
        for u in G:                             # For every from-node...
            for v in G[u]:                      # ... and its to-nodes...
                if relax(G, u, v, D, P):        # Shortcut to v from u?
                    changed = True              # Yes! So something changed
        if not changed: break                   # No change in round: Done
    else:                                       # Not done before round n?
        raise ValueError('negative cycle')      # Negative cycle detected
    return D, P                                 # Otherwise: D and P correct


def test_dijkstra():
    """
    >>> s, t, x, y, z = range(5)
    >>> W = {
    ...     s: {t:10, y:5},
    ...     t: {x:1, y:2},
    ...     x: {z:4},
    ...     y: {t:3, x:9, z:2},
    ...     z: {x:6, s:7}
    ... }
    >>> D, P = dijkstra(W, s)
    >>> [D[v] for v in [s, t, x, y, z]]
    [0, 8, 9, 5, 7]
    >>> s not in P
    True
    >>> [P[v] for v in [t, x, y, z]] == [y, t, s, y]
    True
    """

from heapq import heappush, heappop

def dijkstra(G, s):
    D, P, Q, S = {s:0}, {}, [(0,s)], set()      # Est., tree, queue, visited
    while Q:                                    # Still unprocessed nodes?
        _, u = heappop(Q)                       # Node with lowest estimate
        if u in S: continue                     # Already visited? Skip it
        S.add(u)                                # We've visited it now
        for v in G[u]:                          # Go through all its neighbors
            relax(G, u, v, D, P)                # Relax the out-edge
            heappush(Q, (D[v], v))              # Add to queue, w/est. as pri
    return D, P                                 # Final D and P returned

def test_johnson():
    """
    >>> a, b, c, d, e = range(5)
    >>> W = {
    ...     a: {c:1, d:7},
    ...     b: {a:4},
    ...     c: {b:-5, e:2},
    ...     d: {c:6},
    ...     e: {a:3, b:8, d:-4}
    ... }
    >>> D, P = johnson(W)
    >>> [D[a][v] for v in [a, b, c, d, e]]
    [0, -4, 1, -1, 3]
    >>> [D[b][v] for v in [a, b, c, d, e]]
    [4, 0, 5, 3, 7]
    >>> [D[c][v] for v in [a, b, c, d, e]]
    [-1, -5, 0, -2, 2]
    >>> [D[d][v] for v in [a, b, c, d, e]]
    [5, 1, 6, 0, 8]
    >>> [D[e][v] for v in [a, b, c, d, e]]
    [1, -3, 2, -4, 0]
    """

from copy import deepcopy

def johnson(G):                                 # All pairs shortest paths
    G = deepcopy(G)                             # Don't want to break original
    s = object()                                # Guaranteed unique node
    G[s] = {v:0 for v in G}                     # Edges from s have zero wgt
    h, _ = bellman_ford(G, s)                   # h[v]: Shortest dist from s
    del G[s]                                    # No more need for s
    for u in G:                                 # The weight from u...
        for v in G[u]:                          # ... to v...
            G[u][v] += h[u] - h[v]              # ... is adjusted (nonneg.)
    D, P = {}, {}                               # D[u][v] and P[u][v]
    for u in G:                                 # From every u...
        D[u], P[u] = dijkstra(G, u)             # ... find the shortest paths
        for v in G:                             # For each destination...
            D[u][v] += h[v] - h[u]              # ... readjust the distance
    return D, P                                 # These are two-dimensional

from ch_08 import memo

def test_rec_floyd_warshall():
    """
    >>> a, b, c, d, e = range(1,6) # One-based
    >>> W = {
    ...     a: {c:1, d:7},
    ...     b: {a:4},
    ...     c: {b:-5, e:2},
    ...     d: {c:6},
    ...     e: {a:3, b:8, d:-4}
    ... }
    >>> for u in W:
    ...     for v in W:
    ...         if u == v: W[u][v] = 0
    ...         if v not in W[u]: W[u][v] = inf
    >>> D = rec_floyd_warshall(W)
    >>> [D[a,v] for v in [a, b, c, d, e]]
    [0, -4, 1, -1, 3]
    >>> [D[b,v] for v in [a, b, c, d, e]]
    [4, 0, 5, 3, 7]
    >>> [D[c,v] for v in [a, b, c, d, e]]
    [-1, -5, 0, -2, 2]
    >>> [D[d,v] for v in [a, b, c, d, e]]
    [5, 1, 6, 0, 8]
    >>> [D[e,v] for v in [a, b, c, d, e]]
    [1, -3, 2, -4, 0]
    """

def rec_floyd_warshall(G):                                # All shortest paths
    @memo                                                 # Store subsolutions
    def d(u,v,k):                                         # u to v via 1..k
        if k==0: return G[u][v]                           # Assumes v in G[u]
        return min(d(u,v,k-1), d(u,k,k-1) + d(k,v,k-1))   # Use k or not?
    return {(u,v): d(u,v,len(G)) for u in G for v in G}   # D[u,v] = d(u,v,n)

def test_floyd_warshall1():
    """
    >>> a, b, c, d, e = range(1,6) # One-based
    >>> W = {
    ...     a: {c:1, d:7},
    ...     b: {a:4},
    ...     c: {b:-5, e:2},
    ...     d: {c:6},
    ...     e: {a:3, b:8, d:-4}
    ... }
    >>> for u in W:
    ...     for v in W:
    ...         if u == v: W[u][v] = 0
    ...         if v not in W[u]: W[u][v] = inf
    >>> D = floyd_warshall1(W)
    >>> [D[a][v] for v in [a, b, c, d, e]]
    [0, -4, 1, -1, 3]
    >>> [D[b][v] for v in [a, b, c, d, e]]
    [4, 0, 5, 3, 7]
    >>> [D[c][v] for v in [a, b, c, d, e]]
    [-1, -5, 0, -2, 2]
    >>> [D[d][v] for v in [a, b, c, d, e]]
    [5, 1, 6, 0, 8]
    >>> [D[e][v] for v in [a, b, c, d, e]]
    [1, -3, 2, -4, 0]
    """

def floyd_warshall1(G):
    D = deepcopy(G)                             # No intermediates yet
    for k in G:                                 # Look for shortcuts with k
        for u in G:
            for v in G:
                D[u][v] = min(D[u][v], D[u][k] + D[k][v])
    return D

def test_floyd_warshall():
    """
    >>> a, b, c, d, e = range(5)
    >>> W = {
    ...     a: {c:1, d:7},
    ...     b: {a:4},
    ...     c: {b:-5, e:2},
    ...     d: {c:6},
    ...     e: {a:3, b:8, d:-4}
    ... }
    >>> for u in W:
    ...     for v in W:
    ...         if u == v: W[u][v] = 0
    ...         if v not in W[u]: W[u][v] = inf
    >>> D, P = floyd_warshall(W)
    >>> [D[a][v] for v in [a, b, c, d, e]]
    [0, -4, 1, -1, 3]
    >>> [D[b][v] for v in [a, b, c, d, e]]
    [4, 0, 5, 3, 7]
    >>> [D[c][v] for v in [a, b, c, d, e]]
    [-1, -5, 0, -2, 2]
    >>> [D[d][v] for v in [a, b, c, d, e]]
    [5, 1, 6, 0, 8]
    >>> [D[e][v] for v in [a, b, c, d, e]]
    [1, -3, 2, -4, 0]
    >>> [P[a,v] for v in [a, b, c, d, e]]
    [None, 2, 0, 4, 2]
    >>> [P[b,v] for v in [a, b, c, d, e]]
    [1, None, 0, 4, 2]
    >>> [P[c,v] for v in [a, b, c, d, e]]
    [1, 2, None, 4, 2]
    >>> [P[d,v] for v in [a, b, c, d, e]]
    [1, 2, 3, None, 2]
    >>> [P[e,v] for v in [a, b, c, d, e]]
    [1, 2, 3, 4, None]
    """

def floyd_warshall(G):
    D, P = deepcopy(G), {}
    for u in G:
        for v in G:
            if u == v or G[u][v] == inf:
                P[u,v] = None
            else:
                P[u,v] = u
    for k in G:
        for u in G:
            for v in G:
                shortcut = D[u][k] + D[k][v]
                if shortcut < D[u][v]:
                    D[u][v] = shortcut
                    P[u,v] = P[k,v]
    return D, P

def test_idijkstra():
    """
    >>> s, t, x, y, z = range(5)
    >>> W = {
    ...     s: {t:10, y:5},
    ...     t: {x:1, y:2},
    ...     x: {z:4},
    ...     y: {t:3, x:9, z:2},
    ...     z: {x:6, s:7}
    ... }
    >>> D = dict(idijkstra(W, s))
    >>> [D[v] for v in [s, t, x, y, z]]
    [0, 8, 9, 5, 7]
    """

def idijkstra(G, s):
    Q, S = [(0,s)], set()                       # Queue w/dists, visited
    while Q:                                    # Still unprocessed nodes?
        d, u = heappop(Q)                       # Node with lowest estimate
        if u in S: continue                     # Already visited? Skip it
        S.add(u)                                # We've visited it now
        yield u, d                              # Yield a subsolution/node
        for v in G[u]:                          # Go through all its neighbors
            heappush(Q, (d+G[u][v], v))         # Add to queue, w/est. as pri

def test_bidir_dijkstra_et_al():
    """
    >>> W = {
    ...     'hnl': {'lax':2555},
    ...     'lax': {'sfo':337, 'ord':1743, 'dfw': 1233},
    ...     'sfo': {'ord':1843},
    ...     'dfw': {'ord':802, 'lga':1387, 'mia':1120},
    ...     'ord': {'pvd':849},
    ...     'lga': {'pvd':142},
    ...     'mia': {'lga':1099, 'pvd':1205}
    ... }
    >>> nodes = list(W)
    >>> for u in nodes:
    ...     for v in W[u]:
    ...         if not v in W: W[v] = {}
    ...         W[v][u] = W[u][v]
    ...
    >>> for u in W:
    ...     W[u][u] = 0
    ...
    >>> for u in W:
    ...     for v in W[u]:
    ...         assert W[u][v] == W[v][u]
    ...
    >>> for u in W:
    ...     Dd, _ = dijkstra(W, u)
    ...     Db, _ = bellman_ford(W, u)
    ...     for v in W:
    ...         d = bidir_dijkstra(W, u, v)
    ...         assert d == Dd[v], (d, Dd[v])
    ...         assert d == Db[v], (d, Db[v])
    ...         a = a_star_wrap(W, u, v, lambda v: 0)
    ...         assert a == d
    ...
    >>> G = {0:{0:0}, 1:{1:0}}
    >>> bidir_dijkstra(G, 0, 1)
    inf
    >>> bidir_dijkstra(G, 0, 0)
    0
    >>> G = {0:{1:7}, 1:{0:7}}
    >>> bidir_dijkstra(G, 0, 1)
    7
    >>> bidir_dijkstra(G, 0, 1)
    7
    >>> D, P = dijkstra(W, 'hnl')
    >>> P['pvd'], P['ord'], P['lax']
    ('ord', 'lax', 'hnl')
    >>> D['pvd'] == W['hnl']['lax'] + W['lax']['ord'] + W['ord']['pvd']
    True
    >>> D['pvd']
    5147
    >>> bidir_dijkstra(W, 'hnl', 'pvd')
    5147
    >>> bidir_dijkstra(W, 'pvd', 'sfo')
    2692
    """

from itertools import cycle

def bidir_dijkstra(G, s, t):
    Ds, Dt = {}, {}                             # D from s and t, respectively
    forw, back = idijkstra(G,s), idijkstra(G,t) # The "two Dijkstras"
    dirs = (Ds, Dt, forw), (Dt, Ds, back)       # Alternating situations
    try:                                        # Until one of forw/back ends
        for D, other, step in cycle(dirs):      # Switch between the two
            v, d = next(step)                   # Next node/distance for one
            D[v] = d                            # Demember the distance
            if v in other: break                # Also visite by the other?
    except StopIteration: return inf            # One ran out before they met
    m = inf                                     # They met; now find the path
    for u in Ds:                                # For every visited forw-node
        for v in G[u]:                          # ... go through its neighbors
            if not v in Dt: continue            # Is it also back-visited?
            m = min(m, Ds[u] + G[u][v] + Dt[v]) # Is this path better?
    return m                                    # Return the best path

def a_star(G, s, t, h):
    P, Q = {}, [(h(s), None, s)]                # Pred and queue w/heuristic
    while Q:                                    # Still unprocessed nodes?
        d, p, u = heappop(Q)                    # Node with lowest heuristic
        if u in P: continue                     # Already visited? Skip it
        P[u] = p                                # Set path predecessor
        if u == t: return d - h(t), P           # Arrived! Ret. dist and preds
        for v in G[u]:                          # Go through all neighbors
            w = G[u][v] - h(u) + h(v)           # Modify weight wrt heuristic
            heappush(Q, (d + w, u, v))          # Add to queue, w/heur as pri
    return inf, None                            # Didn't get to t

def a_star_wrap(G, s, t, h):
    return a_star(G, s, t, h)[0]

from string import ascii_lowercase as chars

class WordSpace:                                # An implicit graph w/utils

    def __init__(self, words):                  # Create graph over the words
        self.words = words
        self.M = M = dict()                     # Reachable words

    def variants(self, wd, words):              # Yield all word variants
        wasl = list(wd)                         # The word as a list
        for i, c in enumerate(wasl):            # Each position and character
            for oc in chars:                    # Every possible character
                if c == oc: continue            # Don't replace with the same
                wasl[i] = oc                    # Replace the character
                ow = ''.join(wasl)              # Make a string of the word
                if ow in words:                 # Is it a valid word?
                    yield ow                    # Then we yield it
            wasl[i] = c                         # Reset the character

    def __getitem__(self, wd):                  # The adjacency map interface
        if wd not in self.M:                    # Cache the neighbors
            self.M[wd] = dict.fromkeys(self.variants(wd, self.words), 1)
        return self.M[wd]

    def heuristic(self, u, v):                  # The default heuristic
        return sum(a!=b for a, b in zip(u, v))  # How many characters differ?

    def ladder(self, s, t, h=None):             # Utility wrapper for a_star
        if h is None:                           # Allows other heuristics
            def h(v):
                return self.heuristic(v, t)
        _, P = a_star(self, s, t, h)            # Get the predecessor map
        if P is None:
            return [s, None, t]                 # When no path exists
        u, p = t, []
        while u is not None:                    # Walk backward from t
            p.append(u)                         # Append every predecessor
            u = P[u]                            # Take another step
        p.reverse()                             # The path is backward
        return p

# Some test code you could use:
'''
FORBIDDEN = set("""
dal alod dol aloed algedo elod lod gol geodal dola dogal
""".split())

# This assumes that you have a dictionary in this location, of course:
wds = [line.strip().lower() for line in open("/usr/share/dict/words")]
wds = [wd for wd in wds if wd not in FORBIDDEN]

G = WordSpace(wds)
t0 = time()
print G.ladder(s, t)
print time()-t0

# Should be a lot slower:

G = WordSpace(wds)
t0 = time()
print G.ladder(s, t, h=lambda v: 0)
print time()-t0
'''