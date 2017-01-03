#!/usr/bin/env python

__author__ = "bt3"


def find_0_MxN(m):
    ''' find 0s in a matrix and replace the col and row to 0s:
    >>> m1 = [[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]]
    >>> find_0_MxN(m1)
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]
    >>> m2 = [[1, 2, 3, 4], [0, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]
    >>> find_0_MxN(m2)
    [[0, 2, 3, 4], [0, 0, 0, 0], [0, 10, 11, 12], [0, 14, 15, 16]]
    '''
    index = []

    for row in range(len(m)):
        for col in range(len(m[0])):
            if m[row][col] == 0:
                index.append((row, col))
    for i in index:
        row = i[0]
        col = i[1]
        for i in range(len(m)):
            m[row][i] = 0
        for i in range(len(m[0])):
            m[i][col] = 0

    return m


if __name__ == '__main__':
    import doctest
    doctest.testmod()

