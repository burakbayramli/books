import sys

def test_closest_pair():
    """
    >>> from random import randrange
    >>> from random import seed; seed(2523)
    >>> seq = [randrange(10**5) for i in range(100)]
    >>> dd = float("inf")
    >>> for x in seq:
    ...    for y in seq:
    ...       if x == y: continue
    ...       d = abs(x-y)
    ...       if d < dd:
    ...           xx, yy, dd = x, y, d
    ...
    >>> xx, yy
    (29836, 29825)
    >>> seq.sort()
    >>> dd = float("inf")
    >>> for i in range(len(seq)-1):
    ...     x, y = seq[i], seq[i+1]
    ...     if x == y: continue
    ...     d = abs(x-y)
    ...     if d < dd:
    ...         xx, yy, dd = x, y, d
    ...
    >>> xx, yy
    (29825, 29836)
    """

def test_board():
    """
    >>> board = [[0]*8 for i in range(8)]
    >>> board[7][7] = -1
    >>> cover(board)
    22
    >>> for row in board:
    ...     print((" %2i"*8) % tuple(row))
      3  3  4  4  8  8  9  9
      3  2  2  4  8  7  7  9
      5  2  6  6 10 10  7 11
      5  5  6  1  1 10 11 11
     13 13 14  1 18 18 19 19
     13 12 14 14 18 17 17 19
     15 12 12 16 20 17 21 21
     15 15 16 16 20 20 21 -1
    """

def cover(board, lab=1, top=0, left=0, side=None):
    if side is None: side = len(board)

    # Side length of sub-board:
    s = side // 2

    # Offsets for outer/inner squares of sub-boards:
    offsets = (0, -1), (side-1, 0)

    for dy_outer, dy_inner in offsets:
        for dx_outer, dx_inner in offsets:
            # If the outer corner is not set...
            if not board[top+dy_outer][left+dx_outer]:
                # ... label the inner corner:
                board[top+s+dy_inner][left+s+dx_inner] = lab

    # Next label:
    lab += 1
    if s > 1:
        for dy in [0, s]:
            for dx in [0, s]:
                # Recursive calls, if s is at least 2:
                lab = cover(board, lab, top+dy, left+dx, s)

    # Return the next available label:
    return lab


def test_trav():
    """
    >>> def trav(seq, i=0):
    ...     if i==len(seq): return
    ...     trav(seq, i+1)
    ... 
    >>> trav(range(100))
    """
    # Using range(1000) should give max recursion depth exceeded

def test_recursive_insertion_sort():
    """
    >>> from random import randrange
    >>> seq = [randrange(1000) for i in range(100)]
    >>> seq2 = list(seq)
    >>> ins_sort_rec(seq, len(seq)-1)
    >>> seq == seq2
    False
    >>> seq2.sort()
    >>> seq == seq2
    True
    """

def ins_sort_rec(seq, i):
    if i==0: return                             # Base case -- do nothing
    ins_sort_rec(seq, i-1)                      # Sort 0..i-1
    j = i                                       # Start "walking" down
    while j > 0 and seq[j-1] > seq[j]:          # Look for OK spot
        seq[j-1], seq[j] = seq[j], seq[j-1]     # Keep moving seq[j] down
        j -= 1                                  # Decrement j


def test_insertion_sort():
    """
    >>> from random import *
    >>> seq = [randrange(1000) for i in range(100)]
    >>> seq2 = list(seq)
    >>> ins_sort(seq)
    >>> seq == seq2
    False
    >>> seq2.sort()
    >>> seq == seq2
    True
    """

def ins_sort(seq):
    for i in range(1,len(seq)):                 # 0..i-1 sorted so far
        j = i                                   # Start "walking" down
        while j > 0 and seq[j-1] > seq[j]:      # Look for OK spot
            seq[j-1], seq[j] = seq[j], seq[j-1] # Keep moving seq[j] down
            j -= 1                              # Decrement j

def test_recursive_selection_sort():
    """
    >>> from random import *
    >>> seq = [randrange(1000) for i in range(100)]
    >>> seq2 = list(seq)
    >>> sel_sort_rec(seq, len(seq)-1)
    >>> seq == seq2
    False
    >>> seq2.sort()
    >>> seq == seq2
    True
    """
    
def sel_sort_rec(seq, i):
    if i==0: return                             # Base case -- do nothing
    max_j = i                                   # Idx. of largest value so far
    for j in range(i):                          # Look for a larger value
        if seq[j] > seq[max_j]: max_j = j       # Found one? Update max_j
    seq[i], seq[max_j] = seq[max_j], seq[i]     # Switch largest into place
    sel_sort_rec(seq, i-1)                      # Sort 0..i-1

def test_selection_sort():
    """
    >>> from random import *
    >>> seq = [randrange(1000) for i in range(100)]
    >>> seq2 = list(seq)
    >>> sel_sort(seq)
    >>> seq == seq2
    False
    >>> seq2.sort()
    >>> seq == seq2
    True
    """

def sel_sort(seq):
    for i in range(len(seq)-1,0,-1):            # n..i+1 sorted so far
        max_j = i                               # Idx. of largest value so far
        for j in range(i):                      # Look for a larger value
            if seq[j] > seq[max_j]: max_j = j   # Found one? Update max_j
        seq[i], seq[max_j] = seq[max_j], seq[i] # Switch largest into place


def test_naive_perm():
    """
    >>> M = [2, 2, 0, 5, 3, 5, 7, 4]
    >>> M[2] # c is mapped to a
    0
    >>> sorted(naive_max_perm(M))
    [0, 2, 5]
    """

def naive_max_perm(M, A=None):
    if A is None:                               # The elt. set not supplied?
        A = set(range(len(M)))                  # A = {0, 1, ... , n-1}
    if len(A) == 1: return A                    # Base case -- single-elt. A
    B = set(M[i] for i in A)                    # The "pointed to" elements
    C = A - B                                   # "Not pointed to" elements
    if C:                                       # Any useless elements?
        A.remove(C.pop())                       # Remove one of them
        return naive_max_perm(M, A)             # Solve remaining problem
    return A                                    # All useful -- return all


def test_perm():
    """
    >>> M = [2, 2, 0, 5, 3, 5, 7, 4]
    >>> M[2] # c is mapped to a
    0
    >>> sorted(max_perm(M))
    [0, 2, 5]
    """

def max_perm(M):
    n = len(M)                                  # How many elements?
    A = set(range(n))                           # A = {0, 1, ... , n-1}
    count = [0]*n                               # C[i] == 0 for i in A
    for i in M:                                 # All that are "pointed to"
        count[i] += 1                           # Increment "point count"
    Q = [i for i in A if count[i] == 0]         # Useless elements
    while Q:                                    # While useless elts. left...
        i = Q.pop()                             # Get one
        A.remove(i)                             # Remove it
        j = M[i]                                # Who's it pointing to?
        count[j] -= 1                           # Not anymore...
        if count[j] == 0:                       # Is j useless now?
            Q.append(j)                         # Then deal w/it next
    return A                                    # Return useful elts.

def test_alternate_perm():
    """
    >>> M = [2, 2, 0, 5, 3, 5, 7, 4]
    >>> M[2] # c is mapped to a
    0
    >>> sorted(alternate_max_perm(M))
    [0, 2, 5]
    """

# A test of the tip that says the for loop can be replaced with the use of 
# collections.Counter:
def alternate_max_perm(M):
    # Satisfy the Python 2.6 test run:
    if sys.version <= "3.1": return max_perm(M)
    from collections import Counter
    n = len(M)                                  # How many elements?
    A = set(range(n))                           # A = {0, 1, ... , n-1}
    count = [0]*n                               # C[i] == 0 for i in A
    count = Counter(M)
    Q = [i for i in A if count[i] == 0]         # Useless elements
    while Q:                                    # While useless elts. left...
        i = Q.pop()                             # Get one
        A.remove(i)                             # Remove it
        j = M[i]                                # Who's it pointing to?
        count[j] -= 1                           # Not anymore...
        if count[j] == 0:                       # Is j useless now?
            Q.append(j)                         # Then deal w/it next
    return A                                    # Return useful elts.

def test_counting_sort():
    """
    >>> k = 100
    >>> from random import *
    >>> seq = [randrange(k) for i in range(100)]
    >>> seq2 = list(seq)
    >>> seq = counting_sort(seq) # counting_sort(seq, k)
    >>> seq == seq2
    False
    >>> seq2.sort()
    >>> seq == seq2
    True
    """

def old_counting_sort(A, k):                    # Value range = 0..k-1
    n = len(A)
    B, C = [0]*n, [0]*k                         # Output and counts
    for x in A:
        C[x] += 1                               # Count it
    for x in range(1,k):
        C[x] += C[x-1]                          # Make counts cumulative
    for x in reversed(A):
        C[x] -= 1                               # Find position of x
        B[C[x]] = x                             # Insert x at its position
    return B

from collections import defaultdict

def counting_sort(A, key=lambda x: x):
    B, C = [], defaultdict(list)                # Output and "counts"
    for x in A:
        C[key(x)].append(x)                     # "Count" key(x)
    for k in range(min(C), max(C)+1):           # For every key in the range
        B.extend(C[k])                          # Add values in sorted order
    return B

def test_naive_celeb():
    """
    >>> from random import *
    >>> n = 100
    >>> G = [[randrange(2) for i in range(n)] for i in range(n)]
    >>> c = randrange(n)
    >>> c = 57 # For testing
    >>> for i in range(n):
    ...     G[i][c] = True
    ...     G[c][i] = False
    ...
    >>> naive_celeb(G)
    57
    """

def naive_celeb(G):
    n = len(G)
    for u in range(n):                          # For every candidate...
        for v in range(n):                      # For everyone else...
            if u == v: continue                 # Same person? Skip.
            if G[u][v]: break                   # Candidate knows other
            if not G[v][u]: break               # Other doesn't know candidate
        else:
            return u                            # No breaks? Celebrity!
    return None                                 # Couldn't find anyone

def test_celeb():
    """
    >>> from random import *
    >>> n = 100
    >>> G = [[randrange(2) for i in range(n)] for i in range(n)]
    >>> c = randrange(n)
    >>> c = 57 # For testing
    >>> for i in range(n):
    ...     G[i][c] = True
    ...     G[c][i] = False
    ...
    >>> celeb(G)
    57
    """

def celeb(G):
    n = len(G)
    u, v = 0, 1                                 # The first two
    for c in range(2,n+1):                      # Others to check
        if G[u][v]: u = c                       # u knows v? Replace u
        else:       v = c                       # Otherwise, replace v
    if u == n:      c = v                       # u was replaced last; use v
    else:           c = u                       # Otherwise, u is a candidate
    for v in range(n):                          # For everyone else...
        if c == v: continue                     # Same person? Skip.
        if G[c][v]: break                       # Candidate knows other
        if not G[v][c]: break                   # Other doesn't know candidate
    else:
        return c                                # No breaks? Celebrity!
    return None                                 # Couldn't find anyone

def test_naive_topsort():
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
    >>> sorted = naive_topsort(G)
    >>> rest = set(sorted)
    >>> for u in sorted:
    ...     rest.remove(u)
    ...     assert G[u] <= rest
    ...
    >>> G = {'a': set('bf'), 'b': set('cdf'),
    ... 'c': set('d'), 'd': set('ef'), 'e': set('f'), 'f': set()}
    >>> naive_topsort(G)
    ['a', 'b', 'c', 'd', 'e', 'f']
    """

def naive_topsort(G, S=None):
    if S is None: S = set(G)                    # Default: All nodes
    if len(S) == 1: return list(S)              # Base case, single node
    v = S.pop()                                 # Reduction: Remove a node
    seq = naive_topsort(G, S)                   # Recursion (assumption), n-1
    min_i = 0
    for i, u in enumerate(seq):
        if v in G[u]: min_i = i+1               # After all dependencies
    seq.insert(min_i, v)
    return seq

def test_topsort():
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
    >>> sorted = topsort(G)
    >>> rest = set(sorted)
    >>> for u in sorted:
    ...     rest.remove(u)
    ...     assert G[u] <= rest
    ...
    >>> G = {'a': set('bf'), 'b': set('cdf'),
    ... 'c': set('d'), 'd': set('ef'), 'e': set('f'), 'f': set()}
    >>> topsort(G)
    ['a', 'b', 'c', 'd', 'e', 'f']
    """

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

def test_relax():
    """
    >>> n = 100
    >>> from random import *
    >>> B = dict((i, dict((j, randrange(1000)) for j in range(n)))
    ... for i in range(n))
    >>> for i in range(n):
    ...     B[i][i] = 0
    >>> A = dict((i, randrange(1000)) for i in range(n))
    >>> C = {}
    >>> N = 100
    >>> for v in range(n):
    ...     C[v] = float('inf')
    >>> for i in range(N):
    ...     u, v = randrange(n), randrange(n)
    ...     C[v] = min(C[v], A[u] + B[u][v]) # Relax
    """
