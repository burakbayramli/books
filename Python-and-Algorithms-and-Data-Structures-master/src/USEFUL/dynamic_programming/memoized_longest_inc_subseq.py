#!/usr/bin/python3

__author__ = "bt3"

from itertools import combinations
from bisect import bisect
from memo import memo
from do_benchmark import benchmark

def naive_longest_inc_subseq(seq):
    ''' naive (exponential) solution to the longest increasing subsequence problem '''
    for length in range(len(seq), 0, -1):
        for sub in combinations(seq, length):
            if list(sub) == sorted(sub):
                return len(sub)


def longest_inc_subseq1(seq):
    ''' an iterative algorithm for the longest increasing subsequence problem '''
    end = []
    for val in seq:
        idx = bisect(end, val)
        if idx == len(end): end.append(val)
        else: end[idx] = val
    return len(end)


def longest_inc_subseq2(seq):
    ''' another iterative algorithm for the longest increasing subsequence problem '''
    L = [1] * len(seq)
    for cur, val in enumerate(seq):
        for pre in range(cur):
            if seq[pre] <= val:
                L[cur] = max(L[cur], 1 + L[pre])
    return max(L)


def memoized_longest_inc_subseq(seq):
    ''' a memoized recursive solution to find the longest increasing subsequence problem '''
    @memo
    def L(cur):
        res = 1
        for pre in range(cur):
            if seq[pre] <= seq[cur]:
                res = max(res, 1 + L(pre))
        return res
    return max(L(i) for i in range(len(seq)))


@benchmark
def test_naive_longest_inc_subseq():
    print(naive_longest_inc_subseq(s1))

benchmark
def test_longest_inc_subseq1():
    print(longest_inc_subseq1(s1))

@benchmark
def test_longest_inc_subseq2():
    print(longest_inc_subseq2(s1))

@benchmark
def test_memoized_longest_inc_subseq():
    print(memoized_longest_inc_subseq(s1))


if __name__ == '__main__':
    from random import randrange
    s1 = [randrange(100) for i in range(25)]
    print(s1)
    test_naive_longest_inc_subseq()
    test_longest_inc_subseq1()
    test_longest_inc_subseq2()
    test_memoized_longest_inc_subseq()











