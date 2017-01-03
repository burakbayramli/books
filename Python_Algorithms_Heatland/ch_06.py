def test_heap():
    """
    >>> from heapq import heappush, heappop
    >>> from random import randrange
    >>> Q = []
    >>> for i in range(10):
    ...     heappush(Q, randrange(100))
    ... 
    >>> Q
    [15, 20, 56, 21, 62, 87, 67, 74, 50, 74]
    >>> [heappop(Q) for i in range(10)]
    [15, 20, 21, 50, 56, 62, 67, 74, 74, 87]
    """

# Pseudocode(ish)
def divide_and_conquer(S, divide, combine):
    if len(S) == 1: return S
    L, R = divide(S)
    A = divide_and_conquer(L, divide, combine)
    B = divide_and_conquer(R, divide, combine)
    return combine(A, B)

def test_bisect():
    """
    >>> from bisect import bisect
    >>> a = [0, 2, 3, 5, 6, 8, 8, 9]
    >>> bisect(a, 5)
    4
    >>> from bisect import bisect_left
    >>> bisect_left(a, 5)
    3
    """

# From the Python library, Python 2.3. License issues?
def bisect_right(a, x, lo=0, hi=None):
    if hi is None:                              # Searching to the end
        hi = len(a)
    while lo < hi:                              # More than one possibility
        mid = (lo+hi)//2                        # Bisect (find midpoint)
        if x < a[mid]: hi = mid                 # Value < middle? Go left
        else: lo = mid+1                        # Otherwise: Go right
    return lo

# From the Python library, Python 2.3. License issues?
# Renamed from _siftdown
def sift_up(heap, startpos, pos):
    newitem = heap[pos]                         # The item we're sifting up
    while pos > startpos:                       # Don't go beyond the root
        parentpos = (pos - 1) >> 1              # The same as (pos - 1) // 2
        parent = heap[parentpos]                # Who's your daddy?
        if parent <= newitem: break             # Valid parent found
        heap[pos] = parent                      # Otherwise: Copy parent down
        pos = parentpos                         # Next candidate position
    heap[pos] = newitem                         # Place the item in its spot


# Note: Duplicates are overwritten
def test_binary_tree():
    """
    >>> tree = Tree()
    >>> tree["a"] = 42
    >>> tree["a"]
    42
    >>> "b" in tree
    False
    >>> tree = Tree()
    >>> keys = [4,2,6,1,3,5,7]
    >>> for key in keys:
    ...     tree[key] = str(key)
    ...
    >>> print(bin_tree_str(tree.root))
    4:'4'{2:'2'{1:'1'{*,*},3:'3'{*,*}},6:'6'{5:'5'{*,*},7:'7'{*,*}}}
    >>> tree[6] = "?"
    >>> print(bin_tree_str(tree.root))
    4:'4'{2:'2'{1:'1'{*,*},3:'3'{*,*}},6:'?'{5:'5'{*,*},7:'7'{*,*}}}
    >>> tree[3]
    '3'
    >>> tree[6]
    '?'
    >>> 5 in tree
    True
    >>> 19 in tree
    False
    >>> tree[19]
    Traceback (most recent call last):
        ...
    KeyError
    """


def bin_tree_str(root, chunks=None):
    if chunks is None: chunks = []
    if root is None: chunks.append("*")
    else:
        chunks.append(repr(root.key))
        chunks.append(":")
        chunks.append(repr(root.val))
        chunks.append("{")
        bin_tree_str(root.lft, chunks)
        chunks.append(",")
        bin_tree_str(root.rgt, chunks)
        chunks.append("}")
    return "".join(chunks)

class Node:
    lft = None
    rgt = None
    def __init__(self, key, val):
        self.key = key
        self.val = val

def insert(node, key, val):
    if node is None: return Node(key, val)      # Empty leaf: Add node here
    if node.key == key: node.val = val          # Found key: Replace val
    elif key < node.key:                        # Less than the key?
        node.lft = insert(node.lft, key, val)   # Go left
    else:                                       # Otherwise...
        node.rgt = insert(node.rgt, key, val)   # Go right
    return node

def search(node, key):
    if node is None: raise KeyError             # Empty leaf: It's not here
    if node.key == key: return node.val         # Found key: Return val
    elif key < node.key:                        # Less than the key?
        return search(node.lft, key)            # Go left
    else:                                       # Otherwise...
        return search(node.rgt, key)            # Go right

class Tree:                                     # Simple wrapper
    root = None
    def __setitem__(self, key, val):
        self.root = insert(self.root, key, val)
    def __getitem__(self, key):
        return search(self.root, key)
    def __contains__(self, key):
        try: search(self.root, key)
        except KeyError: return False
        return True

# ----------------------------------------------------------------------------

def aa_tree_str(root, chunks=None):
    if chunks is None: chunks = []
    if root is None: chunks.append("*")
    else:
        chunks.append(repr(root.key))
        chunks.append(repr(root.val))
        chunks.append("@")
        chunks.append(repr(root.lvl))
        chunks.append("{")
        aa_tree_str(root.lft, chunks)
        chunks.append(",")
        aa_tree_str(root.rgt, chunks)
        chunks.append("}")
    return "".join(chunks)

def test_aa_tree():
    """
    >>> root = None
    >>> for key in range(7):
    ...     root = aa_insert(root, key, str(key))
    ...
    >>> print(aa_tree_str(root))
    3'3'@3{1'1'@2{0'0'@1{*,*},2'2'@1{*,*}},5'5'@2{4'4'@1{*,*},6'6'@1{*,*}}}
    """

class AANode:
    lft = None
    rgt = None
    lvl = 1                                     # We've added a level...
    def __init__(self, key, val):
        self.key = key
        self.val = val

def skew(node):                                 # Basically a right rotation
    if None in [node, node.lft]: return node    # No need for a skew
    if node.lft.lvl != node.lvl: return node    # Still no need
    lft = node.lft                              # The 3 steps of the rotation
    node.lft = lft.rgt
    lft.rgt = node
    return lft                                  # Switch pointer from parent

def split(node):                                # Left rotation & level incr.
    if None in [node, node.rgt, node.rgt.rgt]: return node
    if node.rgt.rgt.lvl != node.lvl: return node
    rgt = node.rgt
    node.rgt = rgt.lft
    rgt.lft = node
    rgt.lvl += 1                                # This has moved up
    return rgt                                  # This should be pointed to

def aa_insert(node, key, val):
    if node is None: return AANode(key, val)
    if node.key == key: node.val = val
    elif key < node.key:
        node.lft = aa_insert(node.lft, key, val)
    else:
        node.rgt = aa_insert(node.rgt, key, val)
    node = skew(node)                           # In case it's backward
    node = split(node)                          # In case it's overfull
    return node

# ----------------------------------------------------------------------------

def test_partition_and_select():
    """
    >>> seq = [3, 4, 1, 6, 3, 7, 9, 13, 93, 0, 100, 1, 2, 2, 3, 3, 2]
    >>> partition(seq)
    ([1, 3, 0, 1, 2, 2, 3, 3, 2], 3, [4, 6, 7, 9, 13, 93, 100])
    >>> select([5, 3, 2, 7, 1], 3)
    5
    >>> select([5, 3, 2, 7, 1], 4)
    7
    >>> ans = [select(seq, k) for k in range(len(seq))]
    >>> seq.sort()
    >>> ans == seq
    True
    """

def partition(seq):
    pi, seq = seq[0], seq[1:]                   # Pick and remove the pivot
    lo = [x for x in seq if x <= pi]            # All the small elements
    hi = [x for x in seq if x > pi]             # All the large ones
    return lo, pi, hi                           # pi is "in the right place"

def select(seq, k):
    lo, pi, hi = partition(seq)                 # [<= pi], pi, [> pi]
    m = len(lo)
    if m == k: return pi                        # We found the kth smallest
    elif m < k:                                 # Too far to the left
        return select(hi, k-m-1)                # Remember to adjust k
    else:                                       # Too far to the right
        return select(lo, k)                    # Just use original k here

def test_quicksort():
    """
    >>> seq = [7, 5, 0, 6, 3, 4, 1, 9, 8, 2]
    >>> quicksort(seq)
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    """

def quicksort(seq):
    if len(seq) <= 1: return seq                # Base case
    lo, pi, hi = partition(seq)                 # pi is in its place
    return quicksort(lo) + [pi] + quicksort(hi) # Sort lo and hi separately

def test_mergesort():
    """
    >>> seq = [7, 5, 0, 6, 3, 4, 1, 9, 8, 2]
    >>> mergesort(seq)
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    """

# Mergesort, repeated from Chapter 3 (with some modifications)
def mergesort(seq):
    mid = len(seq)//2                           # Midpoint for division
    lft, rgt = seq[:mid], seq[mid:]
    if len(lft) > 1: lft = mergesort(lft)       # Sort by halves
    if len(rgt) > 1: rgt = mergesort(rgt)
    res = []
    while lft and rgt:                          # Neither half is empty
        if lft[-1] >= rgt[-1]:                  # lft has greatest last value
            res.append(lft.pop())               # Append it
        else:                                   # rgt has greatest last value
            res.append(rgt.pop())               # Append it
    res.reverse()                               # Result is backward
    return (lft or rgt) + res                   # Also add the remainder

def test_slice():
    """
    >>> A = [-1, 2, 1, 0, 4, -3, -6, 1]
    >>> n = len(A)
    >>> max((A[i:j] for i in range(n) for j in range(i+1,n+1)), key=sum)
    [2, 1, 0, 4]
    >>> best = A[0] # A valid solution
    >>> for size in range(1,n+1):
    ...     cur = sum(A[:size])
    ...     for i in range(n-size):
    ...         cur += A[i+size] - A[i]
    ...         best = max(best, cur)
    ...
    >>> best
    7
    """

def test_dsu_bisect():
    """
    >>> from bisect import bisect_left
    >>> seq = "I aim to misbehave".split()
    >>> dec = sorted((len(x), x) for x in seq)
    >>> dec[bisect_left(dec, (3, ""))][1]
    'aim'
    """

def old_test_dsu_bisect():
    """
    >>> from bisect import bisect_left
    >>> seq = "I aim to misbehave".split()
    >>> dec = sorted((len(x), x) for x in seq)
    >>> keys = [k for (k, v) in dec]
    >>> vals = [v for (k, v) in dec]
    >>> vals[bisect_left(keys, 3)]
    'aim'
    >>> vals[bisect_left(keys, 2)]
    'to'
    """
