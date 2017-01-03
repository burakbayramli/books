#!/usr/bin/env python

__author__ = "bt3"


from collections import Counter

def is_anagram(s1, s2):
    '''
    >>> is_anagram('cat', 'tac')
    True
    >>> is_anagram('cat', 'hat')
    False
    '''
    counter = Counter()
    for c in s1:
        counter[c] += 1

    for c in s2:
        counter[c] -= 1

    for i in counter.values():
        if i:
            return False

    return True

''' verify if words are anagrams by comparing a sum of  Unicode code
point of the character'''

def get_unicode_sum(word):
    s = 0
    for p in word:
        s += ord(p)
    return s


def is_anagram2(word1, word2):
    '''
    >>> is_anagram2('cat', 'tac')
    True
    >>> is_anagram2('cat', 'hat')
    False
    '''
    return get_unicode_sum(word1) == get_unicode_sum(word2)


if __name__ == '__main__':
    import doctest
    doctest.testmod()

