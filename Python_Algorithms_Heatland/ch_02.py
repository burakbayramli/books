"""
>>> hash(42)
42
"""
# The same in 2.6 and 3.1 -- but not in 2.7
# >>> hash("Hello, world!")
# -943387004357456228

def test_1():
    n = 1000
    nums = [0]*n
    nums.append(1)
    nums.insert(0,1)

def dump_linked_list(L):
    res = []
    while L is not None:
        res.append(L.value)
        L = L.next
    return res

def test_loop_asymptotics():
    seq = range(10)
    
    s = 0
    for x in seq:
        s += x
        
    assert(s == sum(seq))

    squares = [x**2 for x in seq]

    s = 0
    for x in seq:
        for y in seq:
            s += x*y
        for z in seq:
            for w in seq:
                s += x-w

    seq1 = range(10)
    seq2 = range(5)
    
    s = 0
    for x in seq1:
        for y in seq2:
            s += x*y

    seq1 = [[0, 1], [2], [3, 4, 5]]
    s = 0
    for seq2 in seq1:
        for x in seq2:
            s += x

    seq = range(10)
    s = 0
    n = len(seq)
    for i in range(n-1):
        for j in range(i+1, n):
            s += seq[i] * seq[j]

def test_timeit():
    # Has been tested -- ignored here because timing details vary, and the 
    # tests are slow
    IGNORE = """
    >>> timeit.timeit("x = 2 + 2")
    0.034976959228515625
    >>> timeit.timeit("x = sum(range(10))")
    0.92387008666992188
    """

def test_linked_list():
    """
    >>> Node = test_linked_list()
    >>> L = Node("a", Node("b", Node("c", Node("d"))))
    >>> L.next.next.value
    'c'
    >>> b = L.next
    >>> c = b.next
    >>> b.next = Node("x", c)
    >>> dump_linked_list(L)
    ['a', 'b', 'x', 'c', 'd']
    """
    class Node:
        def __init__(self, value, next=None):
            self.value = value
            self.next = next
    return Node

def test_listing_2_1():
    """
    >>> N = test_listing_2_1()
    >>> a, b, c, d, e, f, g, h = range(8)
    >>> b in N[a]  # Neighborhood membership
    True
    >>> len(N[f])  # Degree
    3
    """
    
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

def test_listing_2_2():
    """
    >>> N = test_listing_2_2()
    >>> a, b, c, d, e, f, g, h = range(8)
    >>> b in N[a]  # Neighborhood membership
    True
    >>> len(N[f])  # Degree
    3
    """
    
    a, b, c, d, e, f, g, h = range(8)
    N = [
        {b, c, d, e, f},    # a
        {c, e},             # b
        {d},                # c
        {e},                # d
        {f},                # e
        {c, g, h},          # f
        {f, h},             # g
        {f, g}              # h
    ]
    
    return N

def test_listing_2_3():
    """
    >>> N = test_listing_2_3()
    >>> a, b, c, d, e, f, g, h = range(8)
    >>> b in N[a]  # Neighborhood membership
    True
    >>> len(N[f])  # Degree
    3
    >>> N[a][b]    # Edge weight for (a, b)
    2
    """
    
    a, b, c, d, e, f, g, h = range(8)
    N = [
        {b:2, c:1, d:3, e:9, f:4},    # a
        {c:4, e:3},                   # b
        {d:8},                        # c
        {e:7},                        # d
        {f:5},                        # e
        {c:2, g:2, h:2},              # f
        {f:1, h:6},                   # g
        {f:9, g:8}                    # h
    ]
    
    return N

def test_listing_2_4():
    """
    >>> N = test_listing_2_4()
    >>> 'b' in N['a']  # Neighborhood membership
    True
    >>> len(N['f'])  # Degree
    3
    """
    
    N = {
        'a': set('bcdef'),
        'b': set('ce'),
        'c': set('d'),
        'd': set('e'),
        'e': set('f'),
        'f': set('cgh'),
        'g': set('fh'),
        'h': set('fg')
    }
    
    return N

def test_listing_2_5():
    """
    >>> N = test_listing_2_5()
    >>> a, b, c, d, e, f, g, h = range(8)
    >>> N[a][b]    # Neighborhood membership
    1
    >>> sum(N[f])  # Degree
    3
    """
    
    a, b, c, d, e, f, g, h = range(8)

    #     a b c d e f g h

    N = [[0,1,1,1,1,1,0,0], # a
         [0,0,1,0,1,0,0,0], # b
         [0,0,0,1,0,0,0,0], # c
         [0,0,0,0,1,0,0,0], # d
         [0,0,0,0,0,1,0,0], # e
         [0,0,1,0,0,0,1,1], # f
         [0,0,0,0,0,1,0,1], # g
         [0,0,0,0,0,1,1,0]] # h
    
    return N

def test_listing_2_6():
    """
    >>> W = test_listing_2_6()
    >>> a, b, c, d, e, f, g, h = range(8)
    >>> inf = float('inf')
    >>> W[a][b] < inf   # Neighborhood membership
    True
    >>> W[c][e] < inf   # Neighborhood membership
    False
    >>> sum(1 for w in W[a] if w < inf) - 1  # Degree
    5
    """

    a, b, c, d, e, f, g, h = range(8)
    _ = float('inf')

    #     a b c d e f g h

    W = [[0,2,1,3,9,4,_,_], # a
         [_,0,4,_,3,_,_,_], # b
         [_,_,0,8,_,_,_,_], # c
         [_,_,_,0,7,_,_,_], # d
         [_,_,_,_,0,5,_,_], # e
         [_,_,2,_,_,0,2,2], # f
         [_,_,_,_,_,1,0,6], # g
         [_,_,_,_,_,9,8,0]] # h
    
    return W

def test_list_tree():
    """
    >>> T = [["a", "b"], ["c"], ["d", ["e", "f"]]]
    >>> T[0][1]
    'b'
    >>> T[2][1][0]
    'e'
    """

def test_listing_2_7():
    """
    >>> Tree = test_listing_2_7()
    >>> t = Tree(Tree("a", "b"), Tree("c", "d"))
    >>> t.right.left
    'c'
    """
    class Tree:
        def __init__(self, left, right=None):
            self.left = left
            self.right = right
    return Tree


def test_listing_2_8():
    """
    >>> Tree = test_listing_2_8()
    >>> t = Tree(Tree("a", Tree("b", Tree("c", Tree("d")))))
    >>> t.kids.next.next.val
    'c'
    """
    class Tree:
        def __init__(self, kids, next=None):
            self.kids = self.val = kids
            self.next = next
    return Tree


def test_bunch():
    """
    >>> Bunch = test_bunch()
    >>> x = Bunch(name="Jayne Cobb", position="Public Relations")
    >>> x.name
    'Jayne Cobb'
    >>> T = Bunch
    >>> t = T(left=T(left="a", right="b"), right=T(left="c"))
    >>> t.left
    {'right': 'b', 'left': 'a'}
    >>> t.left.right
    'b'
    >>> t['left']['right']
    'b'
    >>> "left" in t.right
    True
    >>> "right" in t.right
    False
    """
    class Bunch(dict):
        def __init__(self, *args, **kwds):
            super(Bunch, self).__init__(*args, **kwds)
            self.__dict__ = self
    return Bunch

def test_hidden_squares():
    """
    >>> from random import randrange, seed
    >>> seed(529)
    >>> L = [randrange(10000) for i in range(1000)]
    >>> 42 in L
    False
    >>> S = set(L)
    >>> 42 in S
    False
    >>> input = ["x", "y", "z"]
    >>> s = ""
    >>> for chunk in input:
    ...     s += chunk
    ...
    >>> chunks = []
    >>> for chunk in input:
    ...     chunks.append(chunk)
    ...
    >>> s = ''.join(chunks)
    >>> s = ''.join(input)
    >>> lists = [[42] for i in range(100)]
    >>> res = []
    >>> for lst in lists:
    ...    res.extend(lst)
    >>> res = sum(lists, [])
    """

def test_floats_and_decimals():
    """
    >>> sum(0.1 for i in range(10)) == 1.0
    False
    >>> def almost_equal(x, y, places=7):
    ...     return round(abs(x-y), places) == 0
    >>> almost_equal(sum(0.1 for i in range(10)), 1.0)
    True
    >>> from decimal import *
    >>> sum(Decimal("0.1") for i in range(10)) == Decimal("1.0")
    True
    >>> from math import sqrt
    >>> x = 8762348761.13
    >>> sqrt(x + 1) - sqrt(x)
    5.341455107554793e-06
    >>> 1.0/(sqrt(x + 1) + sqrt(x))
    5.3414570026237696e-06
    """

