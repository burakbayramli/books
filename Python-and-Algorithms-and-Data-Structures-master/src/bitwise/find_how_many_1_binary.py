#!/usr/bin/env python

__author__ = "bt3"

''' Find how many 1s in the binary:
    1) Start with a mask of 1
    2) Mask with AND
    3) if result (if true): count += 1
    (obs: to find the int of a bin do int('1001',
        2)) and to show in bin do bin(int))
'''


def find_how_many_1_in_a_binary(n):
    '''
    >>> find_how_many_1_in_a_binary(9)
    2
    '''

    counter = 0
    while n:
        if n & 1:
            counter += 1
        n >>= 1
    return counter



if __name__ == '__main__':
    import doctest
    doctest.testmod()

