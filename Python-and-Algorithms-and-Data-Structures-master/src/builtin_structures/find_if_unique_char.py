#!/usr/bin/env python

__author__ = "bt3"



import collections

def find_if_unique_chars(word):
    """
    >>> find_if_unique_chars('abcde')
    True
    >>> find_if_unique_chars('abcae')
    False
    """

    unique = True

    counter = collections.Counter()

    for c in word:
        if not counter[c]:
            counter[c] += 1
        else:
            unique = False

    return unique


if __name__ == '__main__':
    import doctest
    doctest.testmod()