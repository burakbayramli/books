#!/usr/bin/env python

__author__ = "bt3"

''' find the longest continuous increasing subsequence'''


def find_long_con_inc(seq):
    '''
    >>> find_long_con_inc([1, -2, 3, 5, 1, -1, 4, -1, 6])
    [-2, 3, 5]
    >>> find_long_con_inc([1, 3, -2, 3, 5, 6])
    [-2, 3, 5, 6]
    >>> find_long_con_inc([1, 3, 4, -13, 2, 5, 8, -1, 2,-17])
    [-13, 2, 5, 8]
    '''

    res, aux = [], []
    seq.append(-float('infinity'))

    for i, n in enumerate(seq[:-1]):
        aux.append(n)
        if n > seq[i+1]:
            if len(res) < len(aux):
                res = aux[:]
            aux = []

    return res



if __name__ == '__main__':
    import doctest
    doctest.testmod()
