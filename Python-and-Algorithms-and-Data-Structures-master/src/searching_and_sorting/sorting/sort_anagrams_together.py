#!/usr/bin/env python

__author__ = "bt3"


''' A method to sort an array so that all the anagrams are together.
    Since we only want the anagrams to be grouped, we can use a
    dictionary for this task. This algorithm is O(n).
'''

from collections import defaultdict

def sort_anagrams_together(l1):
    '''
    >>> l1 = ['hat', 'ball', 'tha', 'cut', 'labl', 'hta', 'cool', 'cuy', 'uct']
    >>> sort_anagrams_together(l1)
    ['cuy', 'cut', 'uct', 'cool', 'ball', 'labl', 'hat', 'tha', 'hta']
    '''
    result = []

    dict_aux = defaultdict(list)
    for word in l1:
        key = ''.join(sorted(word))
        dict_aux[key].append(word)

    for key in dict_aux:
        result.extend(dict_aux[key])

    return result

if __name__ == '__main__':
    import doctest
    doctest.testmod()

