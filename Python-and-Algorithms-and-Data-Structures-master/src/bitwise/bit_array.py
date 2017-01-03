#!/usr/bin/env python

__author__ = "bt3"

''' Example of how to use a bit array in python as a "counter" dict'''

def print_dupl_ba(l1):
    '''
    >>> l1 = [0, 1, 2, 3, 4, 2, 6, 7, 8, 9]
    >>> print_dupl_ba(l1)
    2
    '''

    bs = bytearray(10)
    for i in range(len(l1)):
        if i == l1[i]:
            bs[i] = 1
    for index, bit in enumerate(bs):
        if bit == 0:
            return l1[index]
    return None



if __name__ == '__main__':
    import doctest
    doctest.testmod()

