#!/usr/bin/env python

__author__ = "bt3"


'''
given an array of intervals, return max number of non-overlapping intervals
'''

from collections import defaultdict

def non_overlapping(array):
    '''
    >>> non_overlapping([(1,2), (2,5), (1, 6)])
    [[(1, 2), (2, 5)]]
    >>> non_overlapping([(1,4), (2,5), (3, 6)])
    []
    '''
    total = []

    for i, t in enumerate(array):
        start = t[0]
        end = t[1]
        for tt in array[i+1:] :
            if end <= tt[0]:
                total.append([(start, end), (tt[0], tt[1])])

    return total



if __name__ == '__main__':
    import doctest
    doctest.testmod()

