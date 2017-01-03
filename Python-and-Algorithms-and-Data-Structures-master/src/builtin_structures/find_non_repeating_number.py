#!/usr/bin/env python

__author__ = "bt3"


from collections import defaultdict


def find_unique_number(array):
    '''
    >>> find_unique_number([1, 3, 6, 1, 5, 6, 9, 3, 7])
    [1, 6, 3]
    >>> find_unique_number([1, 3, 5, 6, 9, 7])
    []
    '''

    table = defaultdict()
    total = []

    for i in array:
        if i in table:
            total.append(i)
        else:
            table[i] = 1

    return total




if __name__ == '__main__':
    import doctest
    doctest.testmod()