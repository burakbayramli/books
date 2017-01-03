#!/usr/bin/env python

__author__ = "bt3"

'''Given an array of numbers, replace each number with the product of all
the numbers in the array except the number itself *without* using division
'''


def find_product_without_division(seq):
    '''
    >>> seq = [2,3,4]
    >>> find_product_without_division(seq)
    [12, 8, 6]
    '''

    forw = []
    bacw = []

    for i in range(len(seq)):

        prod_f, prod_b = 1, 1

        for next in range(i+1, len(seq)):
            prod_f *= seq[next]

        for before in range(0, i):
            prod_b *= seq[before]

        forw.append(prod_f)
        bacw.append(prod_b)

    for i in range(len(seq)):
        seq[i] = forw[i] * bacw[i]

    return seq



if __name__ == '__main__':
    import doctest
    doctest.testmod()