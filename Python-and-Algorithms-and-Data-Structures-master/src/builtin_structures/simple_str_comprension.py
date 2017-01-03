#!/usr/bin/env python

__author__ = "bt3"


from collections import Counter

def str_comp(s):
    '''
    >>> s1 = 'aabcccccaaa'
    >>> str_comp(s1)
    'a2b1c5a3'
    >>> str_comp('')
    ''
    '''

    count, last = 1, ''
    list_aux = []
    for i, c in enumerate(s):
        if last == c:
            count += 1
        else:
            if i != 0:
                list_aux.append(str(count))
            list_aux.append(c)
            count = 1
            last = c
    list_aux.append(str(count))
    return ''.join(list_aux)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
