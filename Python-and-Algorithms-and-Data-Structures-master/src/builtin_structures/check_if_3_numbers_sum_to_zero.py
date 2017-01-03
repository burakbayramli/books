#!/usr/bin/env python

__author__ = "bt3"


''' Determine if an Array of integers contains 3 numbers that sum to 0 '''

from collections import defaultdict

def find_3_number(array):
    '''
    >>> find_3_number([1,5,56,11,-3,-12])
    1 11 -12
    True
    >>> find_3_number([] )
    False
    '''
    hash_ = defaultdict()
    for i in array:
        hash_[i] = 1

    for i, x in enumerate(array):
        for y in array[i+1:]:
            if -(x+y) in hash_:
                print x, y, -(x+y)
                return True

    return False




if __name__ == '__main__':
    import doctest
    doctest.testmod()

