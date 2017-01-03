def test_basic_notation():
    """
    >>> from random import *
    >>> seq = S = [randrange(100) for i in range(1000)]
    >>> x = randrange(100)
    >>> x*sum(S) == sum(x*y for y in S)
    True
    >>> def f(i): return i
    >>> def g(i): return 2*i + 42
    >>> m = 0; n = 10
    >>> dummy = sum(f(i) for i in range(m, n+1))
    >>> s = 0
    >>> for i in range(m, n+1):
    ...     s += f(i)
    ...
    >>> X = sum(f(i) for i in seq) + sum(g(i) for i in seq)
    >>> Y = sum(f(i) + g(i) for i in seq)
    >>> X == Y
    True
    """


def test_particles():
    """
    >>> import random; random.seed(42)
    >>> from random import randrange
    >>> n = 10**90
    >>> p = randrange(n)
    >>> p == 52561927548332435090282755894003484804019842420331
    False
    >>> p < n/2
    True
    >>> from math import log
    >>> log(n, 2) #doctest: +ELLIPSIS
    298.9735...
    """


def test_primes():
    """
    >>> is_prime = test_primes()
    >>> is_prime(4)
    False
    >>> is_prime(100)
    False
    >>> is_prime(37)
    True
    """
    def is_prime(n):
        for i in range(2,n):
            if n % i == 0: return False
        return True
    
    return is_prime


def test_recursion():
    """
    >>> from random import *
    >>> seq = [randrange(1000) for i in range(100)]
    >>> S(seq) == sum(seq)
    True
    >>> seq = range(1,101)
    >>> S(seq)
    5050
    """
    
def S(seq, i=0):
    if i == len(seq): return 0
    return S(seq, i+1) + seq[i]

def test_rec_time():
    """
    >>> from random import *
    >>> seq = [randrange(1000) for i in range(100)]
    >>> T(seq) == len(seq) + 1
    True
    >>> seq = range(1,101)
    >>> T(seq)
    101
    >>> for n in range(100):
    ...    seq = range(n)
    ...    assert T(seq) == n+1
    ...
    >>>
    """
    
def T(seq, i=0):
    if i == len(seq): return 1
    return T(seq, i+1) + 1

def test_gnomesort():
    """
    >>> from random import *
    >>> for i in range(10):
    ...     seq = [randrange(1000) for i in range(100)]
    ...     seq2 = sorted(seq)
    ...     gnomesort(seq)
    ...     assert seq == seq2
    ...
    >>>
    """
    
def gnomesort(a):
    i = 0
    while i < len(a):
        if i == 0 or a[i-1] <= a[i]:
            i += 1
        else:
            a[i], a[i-1] = a[i-1], a[i]
            i -= 1

def test_mergesort():
    """
    >>> from random import *
    >>> for i in range(10):
    ...     seq = [randrange(1000) for i in range(100)]
    ...     seq2 = sorted(seq)
    ...     seq = mergesort(seq)
    ...     assert seq == seq2
    ...
    >>>
    """
    
def mergesort(a):
    n = len(a)
    lft, rgt = a[:n//2], a[n//2:]
    if len(lft) > 1: lft = mergesort(lft)
    if len(rgt) > 1: rgt = mergesort(rgt)
    res = []
    while lft and rgt:
        if lft[-1] >= rgt[-1]:
            res.append(lft.pop())
        else:
            res.append(rgt.pop())
    res.reverse()        
    return (lft or rgt) + res
