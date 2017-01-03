#!/usr/bin/env python

__author__ = "bt3"

'''
Given a N different open and close braces in a string "( { [ } ] )".
How do you check whether the string has matching braces.
'''

from collections import Counter
def check_if_balance(string):
    '''
    >>> check_if_balance('{[[(])}]')
    True
    >>> check_if_balance('{[[()}]')
    False
    >>> check_if_balance('')
    True
    '''
    table = Counter()
    for i in string:

        index = str(ord(i))[0]
        if i in '{[(':
            table[index] += 1

        elif i in ')}]':
            table[index] -= 1

    for i in table.values():
        if i !=-0:
            return False
    return True



if __name__ == '__main__':
    import doctest
    doctest.testmod()

