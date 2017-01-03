#!/usr/bin/env python

__author__ = "bt3"

'''  find and delete all the duplicate characters in a string '''

from collections import Counter

def delete_unique(str1):
    '''
    >>> delete_unique("Trust no one")
    'on'
    >>> delete_unique("Mulder?")
    ''
    '''

    str_strip = ''.join(str1.split())
    repeat = Counter()

    for c in str_strip:
        repeat[c] += 1

    result = ''
    for c, count in repeat.items():
        if count > 1:
            result += c

    return result


def removing_duplicates_seq(str1):
    '''
    >>> delete_unique("Trust no one")
    'on'
    >>> delete_unique("Mulder?")
    ''
    '''
    seq = str1.split()
    result = set()
    for item in seq:
        if item not in result:
            #yield item
            result.add(item)
    return result


if __name__ == '__main__':
    import doctest
    doctest.testmod()
