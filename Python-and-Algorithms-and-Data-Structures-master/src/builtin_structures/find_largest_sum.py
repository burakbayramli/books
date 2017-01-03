#!/usr/bin/env python

__author__ = "bt3"



'''
   You are given an array of integers (both positive and negative).
   Find the contiguous sequence with the largest sum.
'''


def find_largest_sum(array):
    '''
    >>> find_largest_sum([-1, 2, -3, 5, 3, 1, -16, 7, 1, -13, 1])
    9
    >>> find_largest_sum([])
    0
    >>> find_largest_sum([1])
    1
    '''

    sum_ = 0
    sum_here = 0

    for i in array:

        sum_here +=  i

        if sum_here < 0:
            sum_here = 0

        if sum_ < sum_here:
            sum_ = sum_here

    return sum_



if __name__ == '__main__':
    import doctest
    doctest.testmod()