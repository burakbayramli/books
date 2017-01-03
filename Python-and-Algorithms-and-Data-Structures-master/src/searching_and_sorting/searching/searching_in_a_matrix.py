#!/usr/bin/env python

__author__ = "bt3"


import numpy

def searching_in_a_matrix(m1, value):
    """ searches an element in a matrix where in every row, the values are increasing from left to
    right, but the last number in a row is smaller than the first number in the next row.
    The naive brute force solution scan all numbers and cost O(nm).  However, since the numbers are
    already sorted, the matrix can be viewed as a 1D sorted array.  The binary search algorithm is
    suitable. The efficience is O(logmn)."""

    rows = len(m1)
    cols = len(m1[0])
    lo = 0
    hi = rows*cols
    while lo < hi:
        mid = (lo + hi)//2
        row = mid//cols
        col = mid%cols
        v = m1[row][col]
        if v == value: return True
        elif v > value: hi = mid
        else: lo = mid+1
    return False



def test_searching_in_a_matrix():
    a = [[1,3,5],[7,9,11],[13,15,17]]
    b = numpy.array([(1,2),(3,4)])
    assert(searching_in_a_matrix(a, 13) == True)
    assert(searching_in_a_matrix(a, 14) == False)
    assert(searching_in_a_matrix(b, 3) == True)
    assert(searching_in_a_matrix(b, 5) == False)
    print('Tests passed!')


if __name__ == '__main__':
    test_searching_in_a_matrix()








