#!/usr/bin/env python

__author__ = "bt3"



def perm(str1):
    '''
    >>> perm('123')
    ['123', '132', '231', '213', '312', '321']
    '''

    if len(str1) < 2:
        return str1

    res = []
    for i, c in enumerate(str1):
        for cc in perm(str1[i+1:] + str1[:i]):
            res.append(c + cc)
    return res


def perm2(str1):
    '''
    >>> perm2('123')
    ['123', '132', '213', '231', '312', '321']
    '''
    from itertools import permutations
    return [''.join(p) for p in permutations(str1)]


def ispermutation(s1, s2):
    '''
    >>> ispermutation('231', '123')
    True
    >>> ispermutation('231', '153')
    False
    '''

    from collections import Counter
    aux = Counter()
    for i in s1:
        aux[i] += 1
    for i in s2:
        aux[i] -= 1
    for v in aux.values():
        if v != 0:
            return False
    return True




def ispermutation2(s1, s2):
    '''
    >>> ispermutation2('231', '123')
    True
    >>> ispermutation2('231', '153')
    False
    '''
    if sorted(s1) == sorted(s2):
        return True
    else:
        return False



if __name__ == '__main__':
    import doctest
    doctest.testmod()
