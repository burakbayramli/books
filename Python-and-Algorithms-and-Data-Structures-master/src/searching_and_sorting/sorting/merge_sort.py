#!/usr/bin/env python

__author__ = "bt3"


def merge_sort(array):
    '''
    >>> merge_sort([3 ,5, 1, 2, 10, 6])
    [1, 2, 3, 5, 6, 10]
    '''
    if len(array) < 2:
        return array

    mid = len(array)//2
    left = merge_sort(array[:mid])
    right = merge_sort(array[mid:])

    res = []
    i, j = 0, 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            res.append(left[i])
            i += 1
        else:
            res.append(right[j])
            j += 1

    if left[i:]:
       res.extend(left[i:])
    if right[j:]:
        res.extend(right[j:])
    return res



''' Merge sort for files '''
def merge_files(list_files):
    result = []
    final = []
    for filename in list_files:
        aux = []
        with open(filename, 'r') as file:
            for line in file:
                aux.append(int(line))
        result.append(aux)
    final.extend(result.pop())
    for l in result:
        final = merge(l, final)
    return final


if __name__ == '__main__':
    import doctest
    doctest.testmod()