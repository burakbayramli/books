#!/usr/bin/python3

__author__ = "bt3"


from functools import wraps
from do_benchmark import benchmark

def memo(func):
    ''' an example of dynamic programming using a memoizing decorator '''
    cache = {}
    @wraps(func)
    def wrap(*args):
        if args not in cache:
            cache[args] = func(*args)
        return cache[args]
    return wrap

@memo
def find_fibonacci_seq_rec(n):
    ''' implements the nth fibonacci value in a recursive exponential runtime '''
    if n < 2: return n
    return find_fibonacci_seq_rec(n - 1) + find_fibonacci_seq_rec(n - 2)

def test_memo():
    n = 50
    # find_fibonacci_seq_rec = memo(find_fibonacci_seq_rec)
    # @benchmark
    print(find_fibonacci_seq_rec(n))


if __name__ == '__main__':
    test_memo()









