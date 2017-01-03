#!/usr/bin/env python

__author__ = "bt3"


def get_products_of_all_except_at_index(array):
    '''
    >>> a = [1, 7, 3, 4]
    >>> get_products_of_all_except_at_index(a)
    [84, 12, 28, 21]
    '''
    total = 1
    for n in array:
        total *= n

    new_array = []
    for n in array:
        if n is not 0:
            item = total/n
            new_array.append(item)
        else:
            new_array.append(n)

    return new_array

if __name__ == '__main__':
    import doctest
    doctest.testmod()