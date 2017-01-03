#!/usr/bin/env python

__author__ = "bt3"

'''
You have two arrays with N integers in them. Merge those arrays using a
recursive algorithm so that the integers in the final array are sorted.
'''

def merge_arrays(a1, a2):
    '''
    >>> merge_arrays([5, 4, 3], [6, 2, 9])
    [2, 3, 4, 5, 6, 9]
    >>> merge_arrays([2, 6], [6, 2])
    [2, 2, 6, 6]
    >>> merge_arrays([], [])
    []
    '''
    # if they are not sorted yet
    a1.sort()
    a2.sort()

    merge = []
    p1, p2 = 0, 0

    while p1 < len(a1) and p2 < len(a2):
        if a1[p1] <= a2[p2]:
            merge.append(a1[p1])
            p1 += 1
        else:
            merge.append(a2[p2])
            p2 +=1

    if a1[p1:]:
        merge.extend(a1[p1:])

    if a2[p2:]:
        merge.extend(a2[p2:])

    return merge


if __name__ == '__main__':
    import doctest
    doctest.testmod()

