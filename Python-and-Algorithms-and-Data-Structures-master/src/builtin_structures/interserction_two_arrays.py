#!/usr/bin/env python

__author__ = "bt3"


def intersection_two_arrays_sets(seq1, seq2):
    '''
    >>> intersection_two_arrays_sets([1,2,3,5,7,8], [3,5,6])
    set([3, 5])
    >>> intersection_two_arrays_sets([1,2,7,8], [3,5,6])
    set([])
    '''
    # O(n)
    set1 = set(seq1)
    set2 = set(seq2)

    return set1.intersection(set2)  #same as list(set1 & set2)


def intersection_two_arrays_On2(seq1, seq2):
    '''
    >>> intersection_two_arrays_On2([1,2,3,5,7,8], [3,5,6])
    [3, 5]
    >>> intersection_two_arrays_On2([1,2,7,8], [3,5,6])
    []
    '''

    final = []

    for i in seq1:
        for j in seq2:
            if i == j:
                final.append(i)

    return final


def intersection_two_arrays_On(seq1, seq2):
    '''
    >>> intersection_two_arrays_On([1,2,3,5,7,8], [3,5,6])
    [5, 3]
    >>> intersection_two_arrays_On([1,2,7,8], [3,5,6])
    []
    '''

    final = []

    while seq1 and seq2:

        if seq1[-1] == seq2[-1]:
            final.append(seq1.pop())
            seq2.pop()
        elif seq1[-1] > seq2[-1]:
            seq1.pop()
        else:
            seq2.pop()

    return final


if __name__ == '__main__':
    import doctest
    doctest.testmod()
