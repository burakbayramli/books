from ch_05 import tr

def test_match():
    """
    >>> G = {
    ... 0: {2, 3},
    ... 1: {3},
    ... 2: set(),
    ... 3: set()
    ... }
    >>> M = match(G, {0, 1}, {2, 3})
    >>> sorted(M)
    [(0, 2), (1, 3)]
    >>> G = {
    ... 0: {3, 4},
    ... 1: {3, 4},
    ... 2: {4},
    ... 3: set(),
    ... 4: set(),
    ... 5: set()
    ... }
    >>> M = match(G, {0, 1, 2}, {3, 4, 5})
    >>> sorted(M)
    [(1, 3), (2, 4)]
    """

from collections import defaultdict
from itertools import chain

def match(G, X, Y):                             # Maximum bipartite matching
    H = tr(G)                                   # The transposed graph
    S, T, M = set(X), set(Y), set()             # Unmatched left/right + match
    while S:                                    # Still unmatched on the left?
        s = S.pop()                             # Get one
        Q, P = {s}, {}                          # Start a traversal from it
        while Q:                                # Discovered, unvisited
            u = Q.pop()                         # Visit one
            if u in T:                          # Finished augmenting path?
                T.remove(u)                     # u is now matched
                break                           # and our traversal is done
            forw = (v for v in G[u] if (u,v) not in M)  # Possible new edges
            back = (v for v in H[u] if (v,u) in M)      # Cancellations
            for v in chain(forw, back):         # Along out- and in-edges
                if v in P: continue             # Already visited? Ignore
                P[v] = u                        # Traversal predecessor
                Q.add(v)                        # New node discovered
        while u != s:                           # Augment: Backtrack to s
            u, v = P[u], u                      # Shift one step
            if v in G[u]:                       # Forward edge?
                M.add((u,v))                    # New edge
            else:                               # Backward edge?
                M.remove((v,u))                 # Cancellation
    return M                                    # Matching -- a set of edges

FF_01_SIMPLE_GRAPH = {
    's': {'u': 1, 'x': 1},
    'u': {'v': 1},
    'v': {'x': 1, 't': 1},
    'x': {'y': 1},
    'y': {'t': 1},
    't': {}
}

def test_01_flow():
    """
    >>> G = FF_01_SIMPLE_GRAPH
    >>> paths(G, 's', 't')
    2
    """

def paths(G, s, t):                             # Edge-disjoint path count
    H, M, count = tr(G), set(), 0               # Transpose, matching, result
    while True:                                 # Until the function returns
        Q, P = {s}, {}                          # Traversal queue + tree
        while Q:                                # Discovered, unvisited
            u = Q.pop()                         # Get one
            if u == t:                          # Augmenting path!
                count += 1                      # That means one more path
                break                           # End the traversal
            forw = (v for v in G[u] if (u,v) not in M)  # Possible new edges
            back = (v for v in H[u] if (v,u) in M)      # Cancellations
            for v in chain(forw, back):         # Along out- and in-edges
                if v in P: continue             # Already visited? Ignore
                P[v] = u                        # Traversal predecessor
                Q.add(v)                        # New node discovered
        else:                                   # Didn't reach t?
            return count                        # We're done
        while u != s:                           # Augment: Backtrack to s
            u, v = P[u], u                      # Shift one step
            if v in G[u]:                       # Forward edge?
                M.add((u,v))                    # New edge
            else:                               # Backward edge?
                M.remove((v,u))                 # Cancellation

FF_SIMPLE_GRAPH = {
    's': {'u': 2, 'x': 2},
    'u': {'v': 1},
    'v': {'x': 1, 't': 2},
    'x': {'y': 1},
    'y': {'t': 2},
    't': {}
}

def path_from_P(P, s, t):
    res = [t]
    u = t
    while u != s:
        u, v = P[u], u
        res.append(u)
    res.reverse()
    return res

def test_bfs_aug():
    """
    >>> G = {0: {1:1}, 1:{}}
    >>> H = tr(G)
    >>> f = defaultdict(int)
    >>> P, c = bfs_aug(G, H, 0, 1, f)
    >>> path_from_P(P, 0, 1)
    [0, 1]
    >>> c
    1
    >>> G = FF_SIMPLE_GRAPH
    >>> H = tr(G)
    >>> f = defaultdict(int)
    >>> f['s','u'] = 1
    >>> f['u','v'] = 1
    >>> f['v','x'] = 1
    >>> f['x','y'] = 1
    >>> f['y','t'] = 1
    >>> P, c = bfs_aug(G, H, 's', 't', f)
    >>> path_from_P(P, 's', 't')
    ['s', 'x', 'v', 't']
    >>> c
    1
    """

from collections import deque
inf = float('inf')

def bfs_aug(G, H, s, t, f):
    P, Q, F = {s: None}, deque([s]), {s: inf}   # Tree, queue, flow label
    def label(inc):                             # Flow increase at v from u?
        if v in P or inc <= 0: return           # Seen? Unreachable? Ignore
        F[v], P[v] = min(F[u], inc), u          # Max flow here? From where?
        Q.append(v)                             # Discovered -- visit later
    while Q:                                    # Discovered, unvisited
        u = Q.popleft()                         # Get one (FIFO)
        if u == t: return P, F[t]               # Reached t? Augmenting path!
        for v in G[u]: label(G[u][v]-f[u,v])    # Label along out-edges
        for v in H[u]: label(f[v,u])            # Label along in-edges
    return None, 0                              # No augmenting path found

def test_ford_fulkerson():
    G = FF_SIMPLE_GRAPH
    f = ford_fulkerson(G, 's', 't')
    print sorted(f.items())
    #[(('s', 'u'), 1), (('s', 'x'), 1), \
#    (('u', 'v'), 1), (('v', 't'), 1), (('v', 'x'), 0), (('x', 'y'), 1), \
#    (('y', 't'), 1)]

def ford_fulkerson(G, s, t, aug=bfs_aug):       # Max flow from s to t
    H, f = tr(G), defaultdict(int)              # Transpose and flow
    while True:                                 # While we can improve things
        P, c = aug(G, H, s, t, f)               # Aug. path and capacity/slack
        if c == 0: return f                     # No augm. path found? Done!
        u = t                                   # Start augmentation
        while u != s:                           # Backtrack to s
            u, v = P[u], u                      # Shift one step
            if v in G[u]: f[u,v] += c           # Forward edge? Add slack
            else:         f[v,u] -= c           # Backward edge? Cancel slack

def busacker_gowen(G, W, s, t):                 # Min-cost max-flow
    def sp_aug(G, H, s, t, f):                  # Shortest path (Bellman-Ford)
        D, P, F = {s:0}, {s:None}, {s:inf,t:0}  # Dist, preds and flow
        def label(inc, cst):                    # Label + relax, really
            if inc <= 0: return False           # No flow increase? Skip it
            d = D.get(u,inf) + cst              # New possible aug. distance
            if d >= D.get(v,inf): return False  # No improvement? Skip it
            D[v], P[v] = d, u                   # Update dist and pred
            F[v] = min(F[u], inc)               # Update flow label
            return True                         # We changed things!
        for rnd in G:                           # n = len(G) rounds
            changed = False                     # No changes in round so far
            for u in G:                         # Every from-node
                for v in G[u]:                  # Every forward to-node
                    changed |= label(G[u][v]-f[u,v], W[u,v])
                for v in H[u]:                  # Every backward to-node
                    changed |= label(f[v,u], -W[v,u])
            if not changed: break               # No change in round: Done
        else:                                   # Not done before round n?
            raise ValueError('negative cycle')  # Negative cycle detected
        return P, F[t]                          # Preds and flow reaching t
    return ford_fulkerson(G, s, t, sp_aug)      # Max-flow with Bellman-Ford

def test_busacker_gowen():    
    G = {
        0: {1:3, 2:3},
        1: {3:2, 4:2},
        2: {3:1, 4:2},
        3: {5:2},
        4: {5:2},
        5: {}
    }
    W = {
    (0,1): 3,
    (0,2): 1,
    (1,3): 1,
    (1,4): 1,
    (2,3): 4,
    (2,4): 2,
    (3,5): 2,
    (4,5): 1
    }
    f1 = ford_fulkerson(G, 0, 5)
    for u, v in W: assert f1[u,v] <= G[u][v]
    print f1[3,5] + f1[4,5]
    print f1[0,1] + f1[0,2]
    f2 = busacker_gowen(G, W, 0, 5)
    for u, v in W: assert f2[u,v] <= G[u][v]
    print sum(f2[key]*W[key] for key in W)
    fs = [f2[key] for key in sorted(W)]
    print fs

#test_ford_fulkerson()

test_busacker_gowen()
