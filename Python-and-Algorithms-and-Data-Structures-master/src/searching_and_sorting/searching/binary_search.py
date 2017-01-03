#!/usr/bin/env python

__author__ = "bt3"


def binary_search(array, item, hi=None, lo=0):
    '''
    >>> binary_search([2,3,5,6,8,10,15,23], 15)
    6
    >>> binary_search([2,3,5,6,8,10,15,23], 4)
    False
    >>> binary_search([1,3,4,5,7,8 ,10,12,23], 10)
    6
    >>> binary_search([1,3,4,5,7,8 ,10,12,23], 22)
    False
    '''

    hi = hi or len(array)
    if hi < lo:
        return False

    mid = (hi + lo)//2
    if  item == array[mid]:
        return mid
    elif item < array[mid]:
        return binary_search(array, item, hi=mid-1, lo=lo)
    else:
        return binary_search(array, item, hi=hi, lo=mid+1)




def binary_search_iter(array, item):
    '''
    >>> binary_search_iter([2,3,5,6,8,10,15,23], 15)
    6
    >>> binary_search_iter([2,3,5,6,8,10,15,23], 4)
    False
    >>> binary_search_iter([1,3,4,5,7,8 ,10,12,23], 10)
    6
    >>> binary_search_iter([1,3,4,5,7,8 ,10,12,23], 22)
    False
    '''
    lo, hi = 0, len(array)

    while lo < hi:
        mid = (hi+lo)//2
        if array[mid] == item:
            return mid
        elif array[mid] > item:
            hi = mid
        else:
            lo=mid+1
    return False








if __name__ == '__main__':
    import doctest
    doctest.testmod()

