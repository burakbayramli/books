#!/usr/bin/env python

__author__ = "bt3"




def insertion_sort(seq):
    ''' sort a sequence using the insertion sort alg '''
    for i in range(1, len(seq)):
        j = i
        while j > 0 and seq[j-1] > seq[j]:
            seq[j-1], seq[j] = seq[j], seq[j-1]
            j -= 1
    return seq


def insertion_sort_rec(seq, i = None):
    ''' sort a sequence using the recursive insertion sort alg '''
    if i == None: i = len(seq) -1
    if i == 0: return i
    insertion_sort_rec(seq, i-1)
    j = i
    while j > 0 and seq[j-i] > seq[j]:
        seq[j-1], seq[j] = seq[j], seq[j-1]
        j -= 1
    return seq


def test_insertion_sort():
    seq = [3, 5, 2, 6, 8, 1, 0, 3, 5, 6, 2, 5, 4, 1, 5, 3]
    assert(insertion_sort(seq) == sorted(seq))
    assert(insertion_sort_rec(seq) == sorted(seq))


if __name__ == '__main__':
    test_insertion_sort()







