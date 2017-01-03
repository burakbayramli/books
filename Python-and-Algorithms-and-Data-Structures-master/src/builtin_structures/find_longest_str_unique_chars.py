#!/usr/bin/env python

__author__ = "bt3"


'''
given a string, find longest string with unique characters
'''

def find_longest(string):
    '''
    >>> find_longest('abfgrhgtrdsandwejfhdasjcbdsjvrejwghireeej')
    'wejfhdas'
    >>> find_longest('abcabcabcabcdefabcccc')
    'defabc'
    '''
    maxs = ''
    result = ''

    for c in string:
        if c in result:
            if len(maxs) < len(result):
                maxs = result
            result = ''
        else:
            result += c

    if result and len(maxs) < len(result):
        maxs = result

    return maxs



if __name__ == '__main__':
    import doctest
    doctest.testmod()

