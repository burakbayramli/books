#!/usr/bin/env python

__author__ = "bt3"


def qs(array):
    '''
    >>> qs([4,1,6,2,7,9,3])
    [1, 2, 3, 4, 6, 7, 9]
    '''
    if len(array) < 2:
        return array

    piv = len(array)//2
    piv_element = array[piv]
    new_array = array[:piv] + array[piv+1:]

    left  = [a for a in new_array if a <= piv_element]
    right = [a for a in new_array if a > piv_element]

    return qs(left) + [array[piv]] + qs(right)



# we can also divide them into two functions
def partition(seq):
    pi,seq = seq[0],seq[1:]
    lo = [x for x in seq if x <= pi]
    hi = [x for x in seq if x > pi]
    return lo, pi, hi

def quick_sort_divided(seq):
    '''
    >>> quick_sort_divided([4,1,6,2,7,9,3])
    [1, 2, 3, 4, 6, 7, 9]
    '''
    if len(seq) < 2:
        return seq
    lo, pi, hi = partition(seq)
    return quick_sort_divided(lo) + [pi] + quick_sort_divided(hi)



if __name__ == '__main__':
    import doctest
    doctest.testmod()