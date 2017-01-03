#!/usr/bin/env python

__author__ = "bt3"


def binary_search_matrix_rec(m, key, lo=0, hi=None):
    '''
    >>> m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    >>> binary_search_matrix_rec(m, 6)
    (1, 2)
    >>> binary_search_matrix_rec(m, 12)
    '''
    if not m:
        return None

    rows = len(m)
    cols = len(m[0])
    hi = hi or rows*cols

    if hi > lo:

        mid = (hi + lo)//2
        row = mid//cols
        col = mid%cols
        item = m[row][col]

        if key == item:
            return row, col
        elif key < item:
            return binary_search_matrix_rec(m, key, lo, mid-1)
        else:
            return binary_search_matrix_rec(m, key, mid+1, hi)

    return None



def binary_search_matrix_iter(m, key):
    '''

    '''

    if not m:
        return None
    rows = len(m)
    cols = len(m[0])
    lo, hi = 0, rows*cols

    while lo < hi:
        mid = (hi + lo)//2
        row = mid//rows
        col = mid%rows
        item = m[row][col]
        if key == item:
            return (row, col)
        elif key < item:
            hi = mid
        else:
            lo = mid +1

    return None


def searching_matrix(m, key):
    '''
    >>> m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    >>> searching_matrix(m, 6)
    (1, 2)
    >>> searching_matrix(m, 12)
    '''

    if not m:
        return None
    rows = len(m)
    cols = len(m[0])
    i, j = 0, cols -1

    while i < rows and j > 0:
        item = m[i][j]
        if key == item:
            return (i, j)
        elif key < item:
            j -= 1
        else:
            i += 1

    return None


if __name__ == '__main__':
    import doctest
    doctest.testmod()
